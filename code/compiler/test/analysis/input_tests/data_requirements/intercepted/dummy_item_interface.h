#pragma once

template<typename DataItem, typename T>
T& data_item_element_access(DataItem& item, const typename DataItem::range_type& range, T& ref) {
	return ref;
}

template<typename DataItem, typename T>
const T& data_item_element_access(const DataItem& item, const typename DataItem::range_type& range, const T& ref) {
	return ref;
}
