module Queue
  (Queue, emptyQ, isEmptyQ , enqueue , firstQ , dequeue)
where 

data Queue = Q [a]

emptyQ :: Queue a 
isEmptyQ :: Queue a -> Bool
enqueue :: a -> Queue a -> Queue a
firstQ :: Queue a -> a
dequeue :: Queue a -> Queue a

emptyQ  = Q []
isEmptyQ (Q qs) = null qs
enqueue x (Q qs) = queueConElementoEnCola x qs 
firstQ (Q qs)  = first qs 
dequeue (Q qs) = sinElPrimero qs 


