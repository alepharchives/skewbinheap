Skew Binomial Heaps
==

Skew Binomial Heaps offer amortized:

    O(1)      insertion
    O(log(N)) merge
    O(log(N)) lowest element
    O(log(N)) removing of lowest element

This implementation is straight from Okasaki's PFDS.

Example
--

    > Heap = skewbinheap:new().
    []

    > H1 = skewbinheap:insert_all([2,1,4,5], Heap).
    [{0,5,[],[]},{1,1,[4],[{0,2,[],[]}]}]

    > skewbinheap:min(H1)
    1

    > skewbinheap:delete_min(H1).
    [{0,4,[],[]},{1,2,[],[{0,5,[],[]}]}] 

    > H2 = skewbinheap:insert_all([100,101], skewbinheap:new()).
    [{0,101,[],[]},{0,100,[],[]}]

    > skewbinheap:merge(H1, H2)
    [{0,5,[],[]},{2,1,[4],[{1,100,[],[{0,101,[],[]}]},{0,2,[],[]}]}]
TODO
--
  
  - Add a custom comparator function
