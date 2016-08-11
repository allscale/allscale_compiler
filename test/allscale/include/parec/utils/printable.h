#pragma once

#include <ostream>
#include <iostream>
#include <type_traits>

namespace parec {
namespace utils {
	class Printable;
	class VirtualPrintable;
}
}

namespace std {

	inline std::ostream& operator<<(std::ostream& out, const parec::utils::VirtualPrintable& printable);

	template<typename T>
	typename std::enable_if<
			std::is_base_of<parec::utils::Printable, T>::value &&
			!std::is_base_of<parec::utils::VirtualPrintable, T>::value
		, std::ostream&>::type
	operator<<(std::ostream& out, const T& printable) {
		return printable.printTo(out);
	}
}

namespace parec {
namespace utils {

	/**
	 * A class forming an interface for printable classes. Implementing this interface allows
	 * classes to be printed to output streams using a member function.
	 */
	class Printable {

		/**
		 * A method to be implemented by sub-classes allowing instances to be printed to the
		 * output stream.
		 *
		 * @param out the stream this instance should be printed to
		 * @return the stream passed as an argument
		 */
		// std::ostream& printTo(std::ostream& out) const { .. }

	};

	/**
	 * A base class for all printable objects within a type hierarchy depending on a virtual printTo function.
	 */
	class VirtualPrintable : public Printable {

		/**
		 * Allow the output operator to access protected members.
		 */
		friend std::ostream& std::operator<<(std::ostream&, const VirtualPrintable&);

	protected:

		virtual ~VirtualPrintable() {}

		/**
		 * A method to be implemented by sub-classes allowing instances to be printed to the
		 * output stream.
		 *
		 * @param out the stream this instance should be printed to
		 * @return the stream passed as an argument
		 */
		virtual std::ostream& printTo(std::ostream& out) const =0;
	};


	/**
	 * A class forming an adapter from class supporting the output operator <<
	 * and classes implementing the printable interface.
	 */
	template<typename T>
	class PrintWrapper : public VirtualPrintable {
		const T& content;
	public:
		PrintWrapper(const T& content) : content(content) {};
		std::ostream& printTo(std::ostream& out) const { return out << content; }
	};

	template<typename T>
	PrintWrapper<T> toVirtualPrintable(const T& element) {
		return PrintWrapper<T>(element);
	}

} // end of namespace utils
} // end of namespace parec

namespace std {

	inline std::ostream& operator<<(std::ostream& out, const parec::utils::VirtualPrintable& printable) {
		return printable.printTo(out);
	}

} // end of namespace std

