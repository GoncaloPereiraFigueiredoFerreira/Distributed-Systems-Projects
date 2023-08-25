# Distributed Systems Projects

This repo contains the two projects developed as part of the "Distributed System Paradigms" and "Large Scale Distributed Systems" courses integrated in our Master's degree in Software Engineering.

## 1st Project: Causal ordering in a reactive system

Grade: **17/20**
Tech-Stack: *Java*, *ReactiveX*

The main objective for this first project was to develop a software component in Java that was capable of ordering reactive events according to their causal order. 

## 2nd Project: Large Scale Storage System

Grade: **20/20**
Tech-Stack: *Java*, *Erlang*, *ZeroMQ*

For the second project, the group developed a key-value storage, that provides a highly scalable and available service.

The system is composed by 3 layers:

![image](https://github.com/GoncaloPereiraFigueiredoFerreira/Distributed-Systems-Projects/assets/62027657/54c9ce66-426b-42b1-800e-01c6fbd981f1)

- **Client Servers** - a java component, design to present an user with an interface to interact with the storage system. It allows 2 operations: single writes and muliple-key reads.
- **Session Servers** - intermediate component that moderates user interactions with the data servers. It was developed in Erlang, in a actor based paradigm, to allow easier scalability.
- **Storage Servers** - the storage servers were developed in Java, and follow the "Chord" arquitecture. It allows runtime scalability and the creation of multiple virtual nodes.



