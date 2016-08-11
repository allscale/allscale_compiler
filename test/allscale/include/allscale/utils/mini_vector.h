#pragma once

namespace allscale {
namespace utils {

	template<typename T>
	class MiniVector {

		void* data;

	public:

		MiniVector() : data(nullptr) {}

		MiniVector(MiniVector&& other)
			: data(other.data) {
			other.data = nullptr;
		}

		~MiniVector() {
			switch(getMark()) {
				case 0: return;
				case 1: delete getData(); return;
				case 7: delete getVector(); return;
				default: delete [] getData();
			}
		}

		std::size_t size() const {
			auto mark = getMark();
			if (mark == 7) {
				return getVector()->size();
			}
			return mark;
		}

		void push_back(const T& element) {

			if (!data) {
				T* content = new T(element);
				data = (void*)(uintptr_t(content) + 1);
				return;
			}

			// for small arrays => copy and insert
			int s = size();
			if (s < 6) {
				T* old = getData();
				T* content = new T[s+1];
				for(int i=0; i<s; i++) {
					content[i] = old[i];
				}
				content[s] = element;
				++s;
				data = (void*)(uintptr_t(content) + s);

				if (s == 2) {
					delete old;
				} else {
					delete [] old;
				}

				// done
				return;
			}

			// if s is getting > 6
			if (s == 6) {

				// copy old to new data
				T* old = getData();
				std::vector<T>* content = new std::vector<T>(old,old+s);
				content->push_back(element);
				assert((uintptr_t(content) % 8) == 0);
				data = (void*)(uintptr_t(content) + 7);

				// delete old data
				delete [] old;

				// done
				return;
			}

			// if it was already a vector => continue
			getVector()->push_back(element);
		}

		const T* begin() const {
			if (isVector()) {
				auto vec = getVector();
				return &(*vec)[0];
			}
			return getData();
		}

		const T* end() const {
			if (isVector()) {
				auto vec = getVector();
				return &(*vec)[vec->size()];
			}
			return getData() + size();
		}

	private:

		int getMark() const {
			uintptr_t value = uintptr_t(data);
			return value & 0x7;
		}

		bool isVector() const {
			return getMark() == 7;
		}

		T* getData() {
			return reinterpret_cast<T*>((void*)(uintptr_t(data) & ~0x7));
		}

		const T* getData() const {
			return reinterpret_cast<const T*>((void*)(uintptr_t(data) & ~0x7));
		}

		std::vector<T>* getVector() {
			return reinterpret_cast<std::vector<T>*>((void*)(uintptr_t(data) & ~0x7));
		}

		const std::vector<T>* getVector() const {
			return reinterpret_cast<const std::vector<T>*>((void*)(uintptr_t(data) & ~0x7));
		}

	};


} // end namespace utils
} // end namespace allscale
