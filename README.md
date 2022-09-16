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

In benchmark.erl navigate to line 5 and add in the details of the Erlang VM and node on which your Injector will reside. Then in faultinjector.erl navigate to line 146 and add in the details of the Erlang VM and node on which your SUB will reside.

You can now compile the code.

And finally, you can run the code but first starting the injector using the command 
```
faultinjector:start(chosen_mode)
```
and then you start the SUB using the commands
```
benchmark:start()
sub ! Number_of_pairs Number_of_sups Failure_frequency {one_for_one, 200000, 1}
```

Note: The information enclosed in {} is details of the supervisor strategy namely: restart_strategy, restart_intensity and restart_interval.

You should now see the system start running.


#### Scala/Akka

First, download the Scala Built Tool (SBT) onto your system from <a href="https://www.scala-sbt.org/">here</a>.
To run the code you also require the JVM.

To configure the benchmark for your system there are some values in the code which you must update.

In App.scala, at lines 19, 20 and 26, 27, add in the IP addresses and port numbers for the SCP and Injector hosts respectively. Furthermore in application.conf again add the details for the two hosts.

You can now compile the code into runnable jar files.

To run the code use the commands (on the respective nodes)
```
SCP.jar inject <mode> 
SCP.jar process <number of supervisors> <number of pairs> <failure rate>
```