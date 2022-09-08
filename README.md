# Supervised Communicating Processes (SCP)

<p align="center">
A benchmark for comparing the reliability of actor-based server languages.
</p>

Developed as part of a successful submission to the Erlang Workshop 2022. Submitted paper can be found on the <a href="https://dl.acm.org/doi/10.1145/3546186.3549928">ACM archive</a>.

</br>

### Benchmark Structure

The benchmark features two main components: the SCP system and the fault injector. The SCP system fatures an array of supervised process pairs with each pair sending messages between each other. The fault injector is a configurable component which allows the user to set a custom failure level and choose between 4 failure modes for injecting failures.

</br>

### Running the Benchmark

The benchmark can be run on a single machine or on two separate nodes or machines.

</br>

#### Erlang

First, download Erlang onto your system from <a href="https://www.erlang.org/downloads">here</a>.

To configure the benchmark for your system there are some values in the code which you must update.


#### Scala/Akka

First, download the Scala Built Tool (SBT) onto your system from <a href="https://www.scala-sbt.org/">here</a>.

To configure the benchmark for your system there are some values in the code which you must update.