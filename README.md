# Throttle

Merely proxies connections to a single server, and throttles the
connection to a given speed in KB/s. This is useful for testing
web sites and other network based applications for which latency
and connection speed can affect the functioning or presentation
of the program to the user.

I don't go to any lengths to ensure the speed is accurate, nor
that asking for 500KB/s will get you 500KB/s, but I do guarantee
if you ask to throttle it at X KB/s then you will not get faster
than X KB/s.

Here is the `--help` output:

    $ throttle --help
    Throttle v1.0, (C) Chris Done 2010

    throttle [OPTIONS]
      Listens on port <listen> and proxies a throttled connection to <host> on
      <port> at speed <speed>KB/s.

    Common flags:
      -l --listen=INT 
      -h --host=ITEM  
      -p --port=INT   
      -s --speed[=NUM]  Speed in KB/s, e.g. 1.6.
      -? --help         Display help message
      -V --version      Print version information

