# Distributed Authenticated Datastream: Overview
## Network

Data is passed downs a tree, with a source of data at the root. The data shares properties with a live broadcast: e.g. one should be able to join anytime, with one authoritative source and many potentially interested listeners.

A trust system is in place to promote the health of the network, placing untrusted nodes closer to the leaves. Trust is gained by providing a high quality rebroadcast. Trust is only one factor in the weight system, allowing for new members of the network to actually gain trust, and for unreliable nodes to regain trust.

Unreliability can be accidental corruption, deliberate corruption (treachery) or leaving the network without warning. 

Nodes should be able to specify how much data they can/want to accept/pass on. 

 - High latency or low data rate should be handled.

## Message format(s)

Downstream: A block of data with a signature from the source, and possibly from the nodes previous.

Upstream: Nodes advertise their load and capability, this data is passed around a bit to keep folks honest, but not too much, to avoid unneeded stress.

Out-of-band: nodes who are joining or leaving communicate with bootstrap nodes, who also communicate with certain other nodes already in the network (to be chosen) to ascertain optimum initial positions. Bootstrap nodes also communicate with each other, but too many bootstrap nodes are more trouble than they solve.

## Connection timeline

1) the data streamer starts the service, communicating openness to rest of wider network
2) first two clients connect, and start receiving data.
3) next clients are connected into the network, arranged into a binary tree, with the streamer at the root.
4) clients self-organise putting the faster nodes closer to the root
5) horizontal arrangement might also be a priority, so as to not have a long  unnecessary bounce.

## Generic ideas

### mathematical challenges

Given a complete, weighted graph with $(number of clients) nodes, calculate the minimum/optimal spanning tree with a given root. 
Update the graph when the weights change, using only local information

### DDoS protection

### other

has to be UDP-based, because streaming and less packets.

## Things that need considering

- Signature size, frequency, guarantee of arrival at same time as data signed.
- how to handle different bitrates.
- How/when to drop chunks to allow for lower data transfer rates
- data privacy (hide IP address from higher nodes if possible)

- **joining, leaving politely, recovery from an abrupt departure.**
	+ polite interruption needs to find new place for all children: one can be replacement, one needs a new place (now kick a parasite)
	+ keep a bit of a backlog in case of promotions (demotions can cause a stutter, but probably won't cause data loss.)
	+ a polite way of leaving: descend the tree until no children or only parasites, and then leave ~abruptly
	+ impolite departures: try to contact grandparent, and do a four-way renegotiation if you can.
		* otherwise: high priority slot request to bootstrap node, n-parents notify bootstrap nodes of new slot.

- network should be able to form even without a data stream. (when stream comes online, everything is ready)
	+ pre-register data stream at bootstrap nodes, with expected start time.

### What if someone drops out rudely.

- children connect to bootstrap nodes 

## stuff

- bootstrap nodes: asked about how to connect to a particular data stream, tell IP addr, etc. Told all the juicy gossip about misbehaving nodes. Told about how much capacity is available for each (re)broadcasting node. connected in a ring,

- We are fine with interception, but not modification of data.