#pragma once

#include <vector>
#include <atomic>
#include <cstdlib>
#include <thread>
#include <mutex>
#include <condition_variable>

#include <pthread.h>

#include "parec/utils/functional_utils.h"
#include "parec/utils/printer/arrays.h"

#include "parec/utils/runtime/lock.h"

/**
 * This header provides a header-only implementation of a minimal
 * runtime for nested parallel tasks.
 */

namespace parec {
namespace utils {
namespace runtime {

	struct Worker;

	thread_local static Worker* tl_worker = nullptr;

	static void setCurrentWorker(Worker& worker) {
		tl_worker = &worker;
	}

	static Worker& getCurrentWorker();

	// -----------------------------------------------------------------
	//						   Immediate
	// -----------------------------------------------------------------


	template<typename T>
	class Immediate {

		T value;

	public:

		Immediate() {}

		Immediate(const T& value) : value(value) {}
		Immediate(T&& value) : value(value) {}

		Immediate(const Immediate&) = delete;
		Immediate(Immediate&& other) = default;

		Immediate& operator=(const Immediate&) = delete;
		Immediate& operator=(Immediate&& other) = default;

		bool isDone() const {
			return true;
		}

		const T& get() const {
			return value;
		}

	};

	template<>
	class Immediate<void> {
	public:

		Immediate() {}

		Immediate(const Immediate&) = delete;
		Immediate(Immediate&& other) = default;

		Immediate& operator=(const Immediate&) = delete;
		Immediate& operator=(Immediate&& other) = default;

		bool isDone() const {
			return true;
		}

		void get() const {
			// nothing
		}

	};

	namespace detail {

		template<
			typename Lambda,
			typename Result
		>
		struct evaluator {
			static Immediate<Result> process(const Lambda& l) {
				return Immediate<Result>(l());
			}
		};

		template<
			typename Lambda
		>
		struct evaluator<Lambda,void> {
			static Immediate<void> process(const Lambda& l) {
				l();
				return Immediate<void>();
			}
		};
	}


	template<
		typename Lambda,
		typename O = typename lambda_traits<Lambda>::result_type
	>
	Immediate<O> evaluate(const Lambda& lambda) {
		return detail::evaluator<Lambda,O>::process(lambda);
	}



	// -----------------------------------------------------------------
	//						Future / Promise
	// -----------------------------------------------------------------

	template<typename T>
	class Future;

	template<>
	class Future<void>;

	template<typename T>
	class Promise;

	template<>
	class Promise<void>;

	template<typename T>
	class FPLink;

	template<>
	class FPLink<void>;


	template<typename T>
	class FPLink {

		friend class Future<T>;

		friend class Promise<T>;

		int ref_counter;

		T value;

		bool done;

	private:

		FPLink() : ref_counter(1), done(false) {}

		FPLink(const T& value)
			: ref_counter(1), value(value), done(true) {}

		void incRef() {
			ref_counter++;
		}

		void decRef() {
			ref_counter--;
			if (ref_counter == 0) delete this;
		}

		bool isDone() const { return done; }

		void setValue(const T& value) {
			this->value = value;
			done = true;
		}

		const T& getValue() const {
			return value;
		}

	};

	template<typename T>
	class Future {

		friend class Promise<T>;

		FPLink<T>* link;

		Future(FPLink<T>* link) : link(link) {
			link->incRef();
		}

	public:

		Future() : link(nullptr) {}

		Future(const T& res) : link(new FPLink<T>(res)) {}

		Future(const Future&) = delete;

		Future(Future&& other) : link(other.link) {
			other.link = nullptr;
		}

		~Future() {
			if (link) link->decRef();
		}

		Future& operator=(const Future&) = delete;

		Future& operator=(Future&& other) {
			if (link == other.link) return *this;
			if (link) link->decRef();
			link = other.link;
			other.link = nullptr;
			return *this;
		}

		bool isDone() const {
			return !link || link->isDone();
		}

		const T& get() const;

	};


	template<typename T>
	class Promise {

		FPLink<T>* link;

	public:

		Promise() : link(new FPLink<T>()) {}

		Promise(const Promise& other) : link(other.link) {
			link->incRef();
		}

		Promise(Promise& other) : link(other.link) {
			other.link = nullptr;
		}

		~Promise() {
			if (link) link->decRef();
		}

		Promise* operator()(const Promise& other) = delete;
		Promise* operator()(Promise&& other) = delete;

		Future<T> getFuture() {
			return Future<T>(link);
		}

		void set(const T& res) {
			link->setValue(res);
		}

	};


	// - void specialization -

	template<>
	class FPLink<void> {

		friend class Future<void>;

		friend class Promise<void>;

		std::atomic<int> ref_counter;

		bool done;

	private:

		FPLink(bool done = false) : ref_counter(1), done(done) {}

		void incRef() {
			ref_counter++;
		}

		void decRef() {
			auto precount = ref_counter.fetch_sub(1);
			if (precount == 1) delete this;
		}

		bool isDone() const { return done; }

	};

	template<>
	class Future<void> {

		template<typename T>
		friend class Promise;

		FPLink<void>* link;

		Future(FPLink<void>* link) : link(link) {
			link->incRef();
		}

	public:

		Future() : link(nullptr) {}

		Future(const Future&) = delete;

		Future(Future&& other) : link(other.link) {
			other.link = nullptr;
		}

		~Future() {
			if (link) link->decRef();
		}

		Future& operator=(const Future&) = delete;

		Future& operator=(Future&& other) {
			if (link == other.link) return *this;
			if (link) link->decRef();
			link = other.link;
			other.link = nullptr;
			return *this;
		}

		bool isDone() const {
			return !link || link->isDone();
		}

		void get() const;

	};


	template<>
	class Promise<void> {

		FPLink<void>* link;

	public:

		Promise() : link(new FPLink<void>()) {}

		Promise(const Promise& other) : link(other.link) {
			link->incRef();
		}

		Promise(Promise& other) : link(other.link) {
			other.link = nullptr;
		}

		~Promise() {
			if (link) link->decRef();
		}

		Promise* operator()(const Promise& other) = delete;
		Promise* operator()(Promise&& other) = delete;

		Future<void> getFuture() {
			return Future<void>(link);
		}

		void set() {
			link->done = true;
		}

	};



	// -----------------------------------------------------------------
	//						    Worker Pool
	// -----------------------------------------------------------------

	using Task = std::function<void()>;


	template<typename T, size_t size>
	class SimpleQueue {

		static const int bsize = size + 1;

		SpinLock lock;

		std::array<T,bsize> data;

		size_t front;
		size_t back;

	public:

		SimpleQueue() : lock(), front(0), back(0) {
			for(auto& cur : data) cur = 0;
		}

		bool empty() const {
			return front == back;
		}
		bool full() const {
			return ((back + 1) % bsize) == front;
		}

		bool push_front(const T& t) {
			lock.lock();
			if (full()) {
				lock.unlock();
				return false;
			}
			front = (front - 1 + bsize) % bsize;
			data[front] = t;
			lock.unlock();
			return true;
		}

		bool push_back(const T& t) {
			lock.lock();
			if (full()) {
				lock.unlock();
				return false;
			}
			data[back] = t;
			back = (back + 1) % bsize;
			lock.unlock();
			return true;
		}

		T pop_front() {
			lock.lock();
			if (empty()) {
				lock.unlock();
				return 0;
			}
			T res = data[front];
			front = (front + 1) % bsize;
			lock.unlock();
			return res;
		}

		T pop_back() {
			lock.lock();
			if (empty()) {
				lock.unlock();
				return 0;
			}
			back = (back - 1 + bsize) % bsize;
			T res = data[back];
			lock.unlock();
			return res;
		}

		friend std::ostream& operator<<(std::ostream& out, const SimpleQueue& queue) {
			return out << "[" << queue.data << "," << queue.front << " - " << queue.back << "]";
		}

	};

	namespace detail {

		/**
		 * A utility to fix the affinity of the current thread to the given core.
		 */
		void fixAffinity(int core) {
			static const int num_cores = std::thread::hardware_concurrency();
			cpu_set_t mask;
			CPU_ZERO(&mask);
			CPU_SET(core % num_cores, &mask);
			pthread_setaffinity_np(pthread_self(), sizeof(cpu_set_t), &mask);
		}

	}

	class WorkerPool;

	struct Worker {

		WorkerPool& pool;

		volatile bool alive;

		SimpleQueue<Task*,8> queue;

		std::thread thread;

		unsigned id;

		unsigned random_seed;

	public:

		Worker(WorkerPool& pool, unsigned id)
			: pool(pool), alive(true), id(id), random_seed(id) { }

		Worker(const Worker&) = delete;
		Worker(Worker&&) = delete;

		Worker& operator=(const Worker&) = delete;
		Worker& operator=(Worker&&) = delete;

		void start() {
			thread = std::thread([&](){ run(); });
		}

		void poison() {
			alive = false;
		}

		void join() {
			thread.join();
		}

	private:

		void run();

	public:

		bool schedule_step();

	};




	class WorkerPool {

		std::vector<Worker*> workers;

		// tools for managing idle threads
		std::mutex m;
		std::condition_variable cv;

		WorkerPool() {

			int numWorkers = std::thread::hardware_concurrency();

			// parse environment variable
			if (char* val = std::getenv("NUM_WORKERS")) {
				auto userDef = std::atoi(val);
				if (userDef != 0) numWorkers = userDef;
			}

			// must be at least one
			if (numWorkers < 1) numWorkers = 1;

			// subtract main thread
			numWorkers = numWorkers - 1;

			// create workers
			for(int i=0; i<numWorkers; ++i) {
				workers.push_back(new Worker(*this, i+1));
			}

			// start workers
			for(auto& cur : workers) cur->start();

			// fix affinity of main thread
			detail::fixAffinity(0);
		}

		~WorkerPool() {
			// shutdown threads

			// poison all workers
			for(auto& cur : workers) {
				cur->poison();
			}

			// signal new work
			workAvailable();

			// wait for their death
			for(auto& cur : workers) {
				cur->join();
			}

			// free resources
			for(auto& cur : workers) {
				delete cur;
			}

		}

	public:

		static WorkerPool& getInstance() {
			static WorkerPool pool;
			return pool;
		}

		int getNumWorkers() const {
			return workers.size();
		}

		Worker& getWorker(int i) {
			return *workers[i];
		}

		Worker& getWorker() {
			static int counter = 0;
			return getWorker((++counter) % workers.size());
		}

	protected:

		friend Worker;

		void waitForWork() {
			std::unique_lock<std::mutex> lk(m);
			cv.wait(lk);
		}

		void workAvailable() {
			// wake up all workers
			cv.notify_all();
		}

	private:

		template<typename LambdaSeq, typename LambdaPar, typename R>
		struct runner {
			Future<R> operator()(Worker& worker, const LambdaSeq& seq, const LambdaPar& par) {

				// if the queue is full, process task immediately
				if (worker.queue.full()) {
					return seq();  // run task and be happy
				}

				// create a schedulable task
				Promise<R> p;
				auto res = p.getFuture();
				Task* t = new Task([=]() mutable {
					p.set(par());
				});

				// schedule task
				bool succ = worker.queue.push_front(t);
				if (!succ) {
					delete t;
					return seq();
				}

				// return future
				return res;
			}
		};

		template<typename LambdaSeq, typename LambdaPar>
		struct runner<LambdaSeq,LambdaPar,void> {
			Future<void> operator()(Worker& worker, const LambdaSeq& seq, const LambdaPar& par) {

				// if the queue is full, process task immediately
				if (worker.queue.full()) {
					seq();			// run task and be happy
					return Future<void>();
				}

				// create a schedulable task
				Promise<void> p;
				auto res = p.getFuture();
				Task* t = new Task([=]() mutable {
					par();
					p.set();
				});

				// schedule task
				bool succ = worker.queue.push_front(t);
				if (!succ) {
					delete t;
					seq();
					return Future<void>();
				}

				// return future
				return res;

			}
		};

		template<typename LambdaSeq, typename R>
		struct direct_runner {
			Future<R> operator()(const LambdaSeq& seq) {
				return seq();
			}
		};

		template<typename LambdaSeq>
		struct direct_runner<LambdaSeq,void> {
			Future<void> operator()(const LambdaSeq& seq) {
				seq();
				return Future<void>();
			}
		};

	public:

		template<typename LambdaSeq, typename LambdaPar, typename R>
		Future<R> spawn(const LambdaSeq& seq, const LambdaPar& par) {

			// check whether there are any workers
			if (getNumWorkers() == 0) {
				// process directly
				return direct_runner<LambdaSeq,R>()(seq);
			}

			// get current worker
			auto& worker = getCurrentWorker();

			// run task
			auto res = runner<LambdaSeq,LambdaPar,R>()(worker, seq, par);

			// make worker aware of new work
			workAvailable();

			// return future
			return res;
		}

	};

	template<typename T>
	const T& Future<T>::get() const {
		while (!isDone()) {
			// process another task
			getCurrentWorker().schedule_step();
		}
		return link->getValue();
	}

	void Future<void>::get() const {
		while (!isDone()) {
			// process another task
			getCurrentWorker().schedule_step();
		}
	}

	inline void Worker::run() {

		// fix affinity
		detail::fixAffinity(id);

		// register worker
		setCurrentWorker(*this);

		// TODO: idle time handling

		// start processing loop
		while(alive) {

			// count number of idle cycles
			int idle_cycles = 0;

			// conduct a schedule step
			while (alive && !schedule_step()) {
				// increment idle counter
				++idle_cycles;

				// wait a moment
				cpu_relax();

				// if there was no work for quite some time
				if (idle_cycles > 1000) {

					// wait for work
					pool.waitForWork();

					// reset cycle counter
					idle_cycles = 0;

				}
			}
		}

		// done

	}

	inline bool Worker::schedule_step() {

		// process a task from the local queue
		if (Task* t = queue.pop_back()) {
			t->operator()();
			delete t;
			return true;
		}

		// check that there are other workers
		int numWorker = pool.getNumWorkers();
		if (numWorker <= 1) return false;

		// otherwise, steal a task from another worker
		Worker& other = pool.getWorker(rand_r(&random_seed) % numWorker);
		if (this == &other) {
			return schedule_step();
		}

		if (Task* t = other.queue.pop_front()) {
			t->operator()();
			delete t;
			return true;
		}

		// no task found => wait a moment
		return false;
	}


	static Worker& getCurrentWorker() {
		if (tl_worker) return *tl_worker;
		return WorkerPool::getInstance().getWorker();
	}

	template<
		typename LambdaSeq,
		typename LambdaPar,
		typename R = typename std::enable_if<
			std::is_same<
				typename lambda_traits<LambdaSeq>::result_type,
				typename lambda_traits<LambdaPar>::result_type
			>::value,
			typename lambda_traits<LambdaSeq>::result_type
		>::type
	>
	Future<R> spawn(const LambdaSeq& seq, const LambdaPar& par) {
		return WorkerPool::getInstance().spawn<LambdaSeq,LambdaPar,R>(seq,par);
	}

	template<
		typename Lambda,
		typename R = typename lambda_traits<Lambda>::result_type
	>
	Future<R> spawn(const Lambda& lambda) {
		return spawn(lambda,lambda);		// only one version => use it for seq and parallel case
	}

} // end namespace runtime
} // end namespace utils
} // end namespace parec
