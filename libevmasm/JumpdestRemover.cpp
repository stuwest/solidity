/*
	This file is part of solidity.

	solidity is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	solidity is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with solidity.  If not, see <http://www.gnu.org/licenses/>.
*/
/**
 * @author Alex Beregszaszi
 * Removes unused JUMPDESTs.
 */

#include "JumpdestRemover.h"

#include <libevmasm/AssemblyItem.h>

using namespace std;
using namespace dev::eth;
using namespace dev;

bool JumpdestRemover::optimise()
{
	unsigned itemCount = m_items.size();

	/// Create set of used tags.
	set<pair<size_t, size_t>> usedTags;
	for (auto const& item: m_items)
		if (item.type() == PushTag)
			usedTags.insert(item.splitForeignPushTag());

	/// Remove tags which are never referenced.
	for (auto it = m_items.begin(); it != m_items.end();)
		/// Tag was never referenced, remove it.
		if (it->type() == Tag && !usedTags.count(it->splitForeignPushTag()))
			it = m_items.erase(it);
		else
			it++;

	return itemCount != m_items.size();
}
