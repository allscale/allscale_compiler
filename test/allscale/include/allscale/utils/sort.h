#pragma once

#include <algorithm>
#include <parec/core.h>

namespace allscale {
namespace utils {

	template<typename Container>
	void sort(Container& c) {
		std::sort(c.begin(),c.end());
	}

	namespace detail {

		#define swap_indices(a, b) \
		{ \
			auto tmp = a;\
			a = b;\
			b = tmp;\
		}

		template<typename Value, typename Iter>
		Iter binsplit(Value val, Iter low, Iter high) {
			/*
			* returns index which contains greatest element <= val.  If val is
			* less than all elements, returns low-1
			*/
			Iter mid;

			while(low != high) {
				mid = low + ((high - low + 1) >> 1);
				if (val <= *mid)
					high = mid - 1;
				else
					low = mid;
			}

			if(*low > val)
				return low - 1;
			else
				return low;
		}


		template<typename Iter, typename Comparator>
		struct merge_params {
			Iter low1, high1, low2, high2, lowdest;
			const Comparator& comp;
		};

		template<typename Iter, typename Comparator>
		void seqmerge(Iter low1, Iter high1, Iter low2, Iter high2, Iter lowdest, const Comparator& comp) {

			/*
			* The following 'if' statement is not necessary
			* for the correctness of the algorithm, and is
			* in fact subsumed by the rest of the function.
			* However, it is a few percent faster.  Here is why.
			*
			* The merging loop below has something like
			*   if (a1 < a2) {
			*        *dest++ = a1;
			*        ++low1;
			*        if (end of array) break;
			*        a1 = *low1;
			*   }
			*
			* Now, a1 is needed immediately in the next iteration
			* and there is no way to mask the latency of the load.
			* A better approach is to load a1 *before* the end-of-array
			* check; the problem is that we may be speculatively
			* loading an element out of range.  While this is
			* probably not a problem in practice, yet I don't feel
			* comfortable with an incorrect algorithm.  Therefore,
			* I use the 'fast' loop on the array (except for the last
			* element) and the 'slow' loop for the rest, saving both
			* performance and correctness.
			*/

			if(low1 < high1 && low2 < high2) {
				auto& a1 = *low1;
				auto& a2 = *low2;
				while(1) {
					if(comp(a1,a2)) {
						*lowdest++ = a1;
						a1 = *++low1;
						if (low1 >= high1)
							break;
					} else {
						*lowdest++ = a2;
						a2 = *++low2;
						if (low2 >= high2)
							break;
					}
				}
			}
			if(low1 <= high1 && low2 <= high2) {
				auto a1 = *low1;
				auto a2 = *low2;
				while(1) {
					if (comp(a1,a2)) {
						*lowdest++ = a1;
						++low1;
						if(low1 > high1)
							break;
						a1 = *low1;
					} else {
						*lowdest++ = a2;
						++low2;
						if (low2 > high2)
							break;
						a2 = *low2;
					}
				}
			}
			if(low1 > high1) {
				for(auto cur = low2; cur < high2; ++cur) {
					*lowdest++ = *cur;
				}
//				memcpy(lowdest, low2, sizeof(ELM) * (high2 - low2 + 1));
			} else {
				for(auto cur = low1; cur < high1; ++cur) {
					*lowdest++ = *cur;
				}
//				memcpy(lowdest, low1, sizeof(ELM) * (high1 - low1 + 1));
			}
		}

		const static int MERGE_REC_CUTOFF = 1024*1024;

		template<typename Iter, typename Comparator, typename MergeFun>
		void cilkmerge_par_step(const merge_params<Iter,Comparator>& p, const MergeFun& merge) {

			Iter low1 = p.low1;
			Iter low2 = p.low2;
			Iter high1 = p.high1;
			Iter high2 = p.high2;
			Iter lowdest = p.lowdest;

			/*
			* Cilkmerge: Merges range [low1, high1] with range [low2, high2]
			* into the range [lowdest, ...]
			*/

			Iter split1, split2;	/*
									* where each of the ranges are broken for
									* recursive merge
									*/
			long int lowsize;		/*
									* total size of lower halves of two
									* ranges - 2
									*/

			/*
			* We want to take the middle element (indexed by split1) from the
			* larger of the two arrays.  The following code assumes that split1
			* is taken from range [low1, high1].  So if [low1, high1] is
			* actually the smaller range, we should swap it with [low2, high2]
			*/

			if(high2 - low2 > high1 - low1) {
				swap_indices(low1, low2);
				swap_indices(high1, high2);
			}

			/*
			* Basic approach: Find the middle element of one range (indexed by
			* split1). Find where this element would fit in the other range
			* (indexed by split 2). Then merge the two lower halves and the two
			* upper halves.
			*/

			split1 = ((high1 - low1 + 1) / 2) + low1;
			split2 = binsplit(*split1, low2, high2);
			lowsize = split1 - low1 + split2 - low2;

			/*
			* directly put the splitting element into
			* the appropriate location
			*/
			*(lowdest + lowsize + 1) = *split1;
			auto f1 = merge({low1, split1 - 1, low2, split2, lowdest, p.comp});
			auto f2 = merge({split1 + 1, high1, split2 + 1, high2, lowdest + lowsize + 2, p.comp});
			f1.get();
			f2.get();
			return;
		}

		template<typename Iter, typename Comparator>
		struct sort_params {
			Iter low;
			Iter tmp;
			long size;
			const Comparator& comp;
		};


		const static int SORT_REC_CUTOFF = 1024*1024;

		template<typename Iter, typename Comparator, typename SortFun, typename MergeFun>
		void cilksort_par_step(const sort_params<Iter,Comparator>& p, const SortFun& sort, const MergeFun& merge) {

			Iter low = p.low;
			Iter tmp = p.tmp;
			long size = p.size;
			auto& comp = p.comp;

			/*
			* divide the input in four parts of the same size (A, B, C, D)
			* Then:
			*   1) recursively sort A, B, C, and D (in parallel)
			*   2) merge A and B into tmp1, and C and D into tmp2 (in parallel)
			*   3) merge tmp1 and tmp2 into the original array
			*/
			long quarter = size / 4;
			Iter A, B, C, D, tmpA, tmpB, tmpC, tmpD;

			if(size < SORT_REC_CUTOFF) {
				/* system sort when less than cutoff elements */
				std::sort(low, low+size, comp);
				return;
			}

			A = low;
			tmpA = tmp;
			B = A + quarter;
			tmpB = tmpA + quarter;
			C = B + quarter;
			tmpC = tmpB + quarter;
			D = C + quarter;
			tmpD = tmpC + quarter;

			auto f1 = sort({A, tmpA, quarter, comp});
			auto f2 = sort({B, tmpB, quarter, comp});
			auto f3 = sort({C, tmpC, quarter, comp});
			auto f4 = sort({D, tmpD, size - 3 * quarter, comp});
			f1.get();
			f2.get();
			f3.get();
			f4.get();

			auto f5 = merge({A, A + quarter - 1, B, B + quarter - 1, tmpA, comp});
			auto f6 = merge({C, C + quarter - 1, D, low + size - 1, tmpC, comp});
			f5.get();
			f6.get();

			merge({tmpA, tmpC - 1, tmpC, tmpA + size - 1, A, comp}).get();
		}


		template<typename Iter, typename Comparator>
		void psort(const Iter& begin, const Iter& end, const Comparator& comp) {

			using sparams = sort_params<Iter,Comparator>;
			using mparams = merge_params<Iter,Comparator>;

			auto rec_qsort = parec::prec(
				parec::fun(													// quick sort
					[](const sparams& p) {
						return p.size < SORT_REC_CUTOFF;
					},
					[](const sparams& p) {
						std::sort(p.low, p.low + p.size, p.comp);
					},
					[](const sparams& p, const auto& sort, const auto& merge) {
						cilksort_par_step(p,sort,merge);
					}
				),
				parec::fun(													// merge step
					[](const mparams& p) {

						Iter low1 = p.low1;
						Iter low2 = p.low2;
						Iter high1 = p.high1;
						Iter high2 = p.high2;
						Iter lowdest = p.lowdest;

						if(high2 - low2 > high1 - low1) {
							swap_indices(low1, low2);
							swap_indices(high1, high2);
						}
						if(high2 < low2) {
							/* smaller range is empty */
							for(auto cur = low1 ; cur != high1; ++cur) {
								*lowdest++ = *cur;
							}
//							memcpy(lowdest, low1, sizeof(ELM) * (high1 - low1));
							return true;
						}
						if(high2 - low2 < MERGE_REC_CUTOFF ) {
							seqmerge(low1, high1, low2, high2, lowdest, p.comp);
							return true;
						}

						return false;
					},
					[](const mparams& p) { },
					[](const mparams& p, const auto&, const auto& merge) {
						cilkmerge_par_step(p,merge);
					}
				)
			);

			// run sorting ...
			rec_qsort({ begin, begin, begin-end, comp }).get();

		}

	}

	template<typename Container>
	void psort(Container& c) {
		detail::psort(c.begin(),c.end(), [](const auto& a, const auto& b) { return a < b; });
//		std::sort(c.begin(),c.end());
	}

} // end namespace utils
} // end namespace allscale
