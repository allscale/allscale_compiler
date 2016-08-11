#pragma once

#include <list>

#include "parec/utils/runtime/lock.h"

namespace allscale {
namespace utils {

	template<typename T>
	class Channel;

	template<typename T>
	class Source {

		friend class Channel<T>;

		Channel<T>* channel;

		Source(Channel<T>* channel) : channel(channel) {}

	public:

		Source() : channel(nullptr) {}

		bool empty() const {
			return !channel || channel->empty();
		}

		T receive() const {
			assert(channel);
			return channel->receive();
		}
	};

	template<typename T>
	class Sink {

		friend class Channel<T>;

		Channel<T>* channel;

		mutable bool clean;

		Sink(Channel<T>* channel) : channel(channel), clean(true) {}

	public:

		Sink() : channel(nullptr), clean(true) {}

		~Sink() { flush(); }

		void send(const T& value) const {
			if (!channel) return;
			channel->send(value);
			clean = false;
		}
		void flush() const {
			if (clean) return;
			if (!channel) return;
			channel->flush();
			clean = true;
		}
	};


	template<typename T>
	class Channel {

		friend class Source<T>;

		friend class Sink<T>;

		parec::utils::runtime::SpinLock lock;
		std::list<T> buffer;

		std::list<T> unflushed;		// for debugging

	public:

		Channel() {}

		Channel(const Channel&) = delete;
		Channel(Channel&&) = delete;

		Channel& operator=(const Channel&) = delete;
		Channel& operator=(Channel&&) = delete;


		Source<T> getSource() {
			return Source<T>(this);
		}

		Sink<T> getSink() {
			return Sink<T>(this);
		}

	private:

		void send(const T& value) {
			lock.lock();
			unflushed.emplace_back(value);
			lock.unlock();
		}

		bool empty() {
			lock.lock();
			bool res = buffer.empty();
			lock.unlock();
			return res;
		}

		T receive() {
			lock.lock();
			if (buffer.empty()) {
				lock.unlock();
				return receive();
			}
			T res = buffer.front();
			buffer.pop_front();
			lock.unlock();
			return res;
		}

		void flush() {
			lock.lock();
			buffer.splice(buffer.end(), unflushed);
			lock.unlock();
		}

	};


} // end namespace utils
} // end namespace allscale
