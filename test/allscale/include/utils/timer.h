#pragma once

#include <string>
#include <chrono>

namespace utils {

	class Timer {

		using clock = std::chrono::high_resolution_clock;
		using time_point = std::chrono::time_point<clock>;
		using duration = typename clock::duration;

		bool running;

		time_point last_start;
		duration total_time;

		std::string name;

	public:

		Timer(const std::string& name = "")
			: running(false), total_time(0), name(name) {}

		void start() {
			if (running) return;
			running = true;
			last_start = now();
		}

		void stop() {
			if (!running) return;
			running = false;
			total_time += now() - last_start;
		}

		const duration& getTotalTime() const {
			return total_time;
		}

		uint32_t getTotalTimeInMilliseconds() const {
			return std::chrono::duration_cast<std::chrono::milliseconds>(total_time).count();
		}

	private:

		time_point now() {
			return clock::now();
		}


		friend std::ostream& operator<<(std::ostream& out, const Timer& timer) {
			if (!timer.name.empty()) {
				out << timer.name << ": ";
			}
			out << timer.getTotalTimeInMilliseconds() << "ms";
			if (timer.running) {
				out << " [RUNNING]";
			}
			return out;
		}

	};


} // end namespace utils
