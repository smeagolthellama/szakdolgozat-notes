# Overview
## Network
## Message format
## Connection timeline

1) the data streamer starts the service, communicating openness to rest of wider network
2) first two clients connect, and start receiving data.
3) next clients are connected into the network, arranged into a binary tree, with the streamer at the root.
4) clients self-organise putting the faster nodes closer to the root
5) horizontal arrangement might also be a priority, so as to not have a long  unnecessary bounce.

## generic ideas

### mathematical challenges

Given a complete, weighted graph with $(number of clients) nodes, calculate the minimum spanning binary tree with a given root. 
Update the graph when the weights change, using only local information