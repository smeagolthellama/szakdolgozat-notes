# Distributed Authenticated Datastream: Overview
## Network

Data is passed downs a tree, with a source of data at the root. The data shares properties with a live broadcast: e.g. one should be able to join anytime, with one authoritative source and many potentially interested listeners.

A trust system is in place to promote the health of the network, placing untrusted nodes closer to the leaves. Trust is gained by providing a high quality rebroadcast. Trust is only one factor in the weight system, allowing for new members of the network to actually gain trust, and for unreliable nodes to regain trust.

Unreliability can be accidental corruption, deliberate corruption (treachery) or leaving the network without warning. 

Nodes should be able to specify how much data they can/want to accept/pass on. 

 - High latency or low data rate should be handled.

## Message format

A block of data with a signature from the source, and possibly from the nodes previous.

## Connection timeline

1) the data streamer starts the service, communicating openness to rest of wider network
2) first two clients connect, and start receiving data.
3) next clients are connected into the network, arranged into a binary tree, with the streamer at the root.
4) clients self-organise putting the faster nodes closer to the root
5) horizontal arrangement might also be a priority, so as to not have a long  unnecessary bounce.

## Generic ideas

### mathematical challenges

Given a complete, weighted graph with $(number of clients) nodes, calculate the minimum/optimal spanning binary tree with a given root. 
Update the graph when the weights change, using only local information

### DDoS protection

### other

has to be UDP-based, because streaming and less packets.

## Things that need considering

- Signature size, frequency, guarantee of arrival at same time as data signed.
- how to handle different bitrates.
- How/when to drop chunks to allow for lower data transfer rates

## Options
