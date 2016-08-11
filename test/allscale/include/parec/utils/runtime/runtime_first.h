#pragma once

#include <atomic>
#include <thread>

#include "parec/utils/functional_utils.h"
#include "parec/utils/printer/arrays.h"

namespace parec {
namespace util {
namespace runtime {

	/* Pause instruction to prevent excess processor bus usage */
	#define cpu_relax() asm volatile("pause\n": : :"memory")

	class Waiter {
		int i;
	public:
		Waiter() : i(0) {}

		void operator()() {
			++i;
			if ((i % 1000) == 0) {
				// there was no progress => let others work
				std::this_thread::yield();
			} else {
				// relax this CPU
				cpu_relax();
			}
		}
	};



    class SpinLock {
        std::atomic<int> lck;
    public:

        SpinLock() : lck(0) {
        }

        void lock() {
            Waiter wait;
            while(!try_lock()) wait();
        }

        bool try_lock() {
            int should = 0;
            return lck.compare_exchange_weak(should, 1, std::memory_order_acquire);
        }

        void unlock() {
            lck.store(0, std::memory_order_release);
        }
    };

	struct Worker;

	thread_local static Worker* tl_worker = nullptr;

	static void setCurrentWorker(Worker& worker) {
		tl_worker = &worker;
	}

	static Worker& getCurrentWorker();


	template<typename T>
	class Future;

	template<>
	class Future<void>;

	template<typename T>
	class Promise;

	template<>
	class Promise<void>;


	template<typename T>
	class Promise {

		friend class Future<T>;

		Future<T>* future;

	public:

		Promise() : future(nullptr) {}

		Promise(const Promise& other) {
			if (other.future) {
				future = other.future;
				future->promise = this;
			}
		}

		Promise(Promise&& other) {
			if (other.future) {
				future = other.future;
				future->promise = this;
			}
		}

		Promise& operator=(const Promise&) = delete;
		Promise& operator=(Promise&& other) = delete;

		Future<T> getFuture() {
			return Future<T>(*this);
		}

		void set(const T& res);

	private:

		void updateFuture(Future<T>* future) {
			this->future = future;
		}

	};


	template<typename T>
	class Future {

		T res;

		std::atomic<Promise<T>*> promise;

		std::atomic_flag msg_lck;

		Future(Promise<T>& promise)
			: promise(&promise), msg_lck(false) {
			promise.updateFuture(this);
		}

	public:

		Future(const T& res)
			: res(res), promise(nullptr), msg_lck(false) {}

		Future(const Future&) = delete;

		Future(Future&& other)
			: promise(nullptr), msg_lck(false) {

			// lock other node (to not miss incoming messages)
			other.msg_lock();

			// migrate link to promise
			auto ptr = other.promise.load(std::memory_order_relaxed);
			promise.store(ptr, std::memory_order_relaxed);
			if (ptr) {
				ptr->updateFuture(this);
			} else {
				res = other.res;
			}

			other.promise.store(nullptr, std::memory_order_relaxed);

		}

		~Future() {
			separate();
		}

		Future& operator=(const Future&) = delete;

		Future& operator=(Future&& other) {
			// separate link to current promise
			separate();

			// lock other node (to not miss incoming messages)
			other.msg_lock();

			// migrate link to promise
			auto ptr = other.promise.load(std::memory_order_relaxed);
			promise.store(ptr, std::memory_order_relaxed);
			if (ptr) {
				ptr->updateFuture(this);
			} else {
				res = other.res;
			}

			other.promise.store(nullptr, std::memory_order_relaxed);

			return *this;
		}


		bool isDone() const {
			return promise == nullptr;
		}

		const T& get() const;

	private:

		friend class Promise<T>;

		bool try_msg_lock() {
			return !msg_lck.test_and_set(std::memory_order_acquire);
		}

		void msg_lock() {
			while(msg_lck.test_and_set(std::memory_order_acquire)) {
				// spin
				cpu_relax();
			}
		}

		void msg_unlock() {
			msg_lck.clear(std::memory_order_release);
		}

		void separate() {
			if (promise.load()) {
				msg_lock();
				if(auto ptr = promise.load()) ptr->updateFuture(nullptr);
				msg_unlock();
			}
		}

		void set(const T& value) {
			res = value;
			promise.store(nullptr, std::memory_order_release);
		}

	};


	template<typename T>
	void Promise<T>::set(const T& res) {
		while (Future<T>* f = future) {
			if (!f->try_msg_lock()) continue;
			f->set(res);
			f->msg_unlock();
			future = nullptr;
		}
	}

	template<>
	class Promise<void> {

		friend class Future<void>;

		Future<void>* future;

	public:

		Promise() : future(nullptr) {}

		Promise(Promise&& other);

		Promise(const Promise&);

//		~Promise() {
//			if (auto f = future.load()) {
//				f.msg_lock();
//				f.setPromise(nullptr);
//				f.msg_unlock();
//			}
//		}

		Promise& operator=(const Promise&) = delete;
		Promise& operator=(Promise&& other) = delete;

		Future<void> getFuture();

		void set();

	private:

		void updateFuture(Future<void>* future) {
			this->future = future;
		}

	};


	template<>
	class Future<void> {

		std::atomic<Promise<void>*> promise;

		std::atomic_flag msg_lck;

		Future(Promise<void>& promise)
			: promise(&promise), msg_lck(false) {
			promise.updateFuture(this);
		}

	public:

		Future()
			: promise(nullptr), msg_lck(false) {}

		Future(const Future&) = delete;

		Future(Future&& other)
			: promise(nullptr), msg_lck(false) {

			// lock other node (to not miss incoming messages)
			other.msg_lock();

			// migrate link to promise
			auto ptr = other.promise.load(std::memory_order_relaxed);
			promise.store(ptr, std::memory_order_relaxed);
			if (ptr) {
				ptr->updateFuture(this);
			}

			// separate other future from promise
			other.promise = nullptr;
		}

		~Future() {
			separate();
		}

		Future& operator=(const Future&) = delete;

		Future& operator=(Future&& other) {
			// separate link to current promise
			separate();

			// lock other node (to not miss incoming messages)
			other.msg_lock();

			// migrate link to promise
			auto ptr = other.promise.load(std::memory_order_relaxed);
			promise.store(ptr, std::memory_order_relaxed);
			if (ptr) {
				ptr->updateFuture(this);
			}

			// remove promis link in other
			other.promise = nullptr;

			return *this;
		}


		bool isDone() const {
			return promise == nullptr;
		}

		void get() const;

	private:

		friend class Promise<void>;

		bool try_msg_lock() {
			return !msg_lck.test_and_set(std::memory_order_acquire);
		}

		void msg_lock() {
			while(msg_lck.test_and_set(std::memory_order_acquire)) {
				// spin
				cpu_relax();
			}
		}

		void msg_unlock() {
			msg_lck.clear(std::memory_order_release);
		}

		void separate() {
			if (promise.load()) {
				msg_lock();
				if(auto ptr = promise.load()) ptr->updateFuture(nullptr);
				msg_unlock();
			}
		}

		void set() {
			promise.store(nullptr, std::memory_order_release);
		}

	};

	Promise<void>::Promise(const Promise& other) {
		if (other.future) {
			future = other.future;
			future->promise.store(this, std::memory_order_relaxed);
		}
	}

	Promise<void>::Promise(Promise&& other) {
		if (other.future) {
			future = other.future;
			future->promise.store(this, std::memory_order_relaxed);
			other.future = nullptr;
		}
	}

	Future<void> Promise<void>::getFuture() {
		return Future<void>(*this);
	}

	void Promise<void>::set() {
		while (Future<void>* f = future) {
			if (!f->try_msg_lock()) continue;
//			if (f != future) continue;
//			std::cout << "sending message from p=" << this << " to f=" << future << "\n";
			f->set();
			f->msg_unlock();
			future = nullptr;
		}
	}

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

	class WorkerPool;

	struct Worker {

		WorkerPool& pool;

		volatile bool alive;

		SimpleQueue<Task*,8> queue;

		std::thread thread;

	public:

		Worker(WorkerPool& pool)
			: pool(pool), alive(true) { }

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

		void run() {

			// register worker
			setCurrentWorker(*this);

			// TODO: sync work queue + steeling + idle time handling

			// start processing loop
			while(alive) {

				// conduct a schedule step
				schedule_step();
//				// process tasks in queue
//				while(Task* t = queue.pop_back()) {
//					t->operator()();		// run task
//					delete t;
//				}
			}

			// terminate

		}

	public:

		void schedule_step();

	};




	class WorkerPool {

		std::vector<Worker*> workers;


		WorkerPool() {

//			std::cout << "Creating " << std::thread::hardware_concurrency() << " threads!\n";
			for(unsigned i=0; i<std::thread::hardware_concurrency(); ++i) {
				workers.push_back(new Worker(*this));
			}

			for(auto& cur : workers) cur->start();
		}

		~WorkerPool() {
			// shutdown threads

			// poison all workers
			for(auto& cur : workers) {
//				std::cout << "Poisoning worker " << &cur << "\n";
				cur->poison();
			}

			// wait for their death
			for(auto& cur : workers) {
				cur->join();
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

		Worker& getWorker(int i = 0) {
			return *workers[i];
		}

	private:

		template<typename Lambda, typename R>
		struct runner {
			Future<R> operator()(Worker& worker, const Lambda& task) {

				// if the queue is full, process task immediately
				if (worker.queue.full()) {
					return task();  // run task and be happy
				}


				// create a schedulable task
				Promise<R> p;
				auto res = p.getFuture();
				Task* t = new Task([=]() mutable {
					p.set(task());
				});

				// schedule task
				bool succ = worker.queue.push_front(t);
				if (!succ) {
					delete t;
					return task();
				}

				// return future
				return res;
			}
		};

		template<typename Lambda>
		struct runner<Lambda,void> {
			Future<void> operator()(Worker& worker, const Lambda& task) {

				// if the queue is full, process task immediately
				if (worker.queue.full()) {
					task();			// run task and be happy
					return Future<void>();
				}

				// create a schedulable task
				Promise<void> p;
				auto res = p.getFuture();
				Task* t = new Task([=]() mutable {
//					std::cout << "starting task ..\n";
					task();
					p.set();
//					std::cout << "ending task ..\n";
				});

				// schedule task
				bool succ = worker.queue.push_front(t);
				if (!succ) {
					delete t;
					task();
					return Future<void>();
				}

				// return future
				return res;

			}
		};

	public:

		template<typename Lambda, typename R>
		Future<R> spawn(const Lambda& lambda) {

			// get current worker
			auto& worker = getCurrentWorker();

			// TODO: handle spawn call from non-thread

			return runner<Lambda,R>()(worker, lambda);
		}

	};

	template<typename T>
	const T& Future<T>::get() const {
		while (!isDone()) {
			// process another task
			getCurrentWorker().schedule_step();
		}
		return res;
	}

	void Future<void>::get() const {
		while (!isDone()) {
			// process another task
			getCurrentWorker().schedule_step();
		}
	}

	inline void Worker::schedule_step() {

		// process a task from the local queue
		if (Task* t = queue.pop_back()) {
			t->operator()();
			delete t;
			return;
		}

//		// otherwise, steal a task from another worker
//		Worker& other = pool.getWorker(rand() % pool.getNumWorkers());
//		if (this == &other) {
//			schedule_step();
//			return;
//		}
//
//		if (Task* t = other.queue.pop_front()) {
//			t->operator()();
//			delete t;
//			return;
//		}
	}


	static Worker& getCurrentWorker() {
		if (tl_worker) return *tl_worker;
		return WorkerPool::getInstance().getWorker();
	}

	template<
		typename Lambda,
		typename R = typename lambda_traits<Lambda>::result_type
	>
	Future<R> spawn(const Lambda& lambda) {
		return WorkerPool::getInstance().spawn<Lambda,R>(lambda);
	}


} // end namespace runtime
} // end namespace util
} // end namespace parec
