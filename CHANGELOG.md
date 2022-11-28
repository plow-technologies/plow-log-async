# Revision history for plow-log-async

## 0.1.4.0
* Prep for OSS release

## 0.1.3.0
* Upgraded to work with GHC 9

## 0.1.2.0 -- 2021-7-226

* Fixed adding timestamp on tracer call not when the message is printed from the queue
* Fixed withAsyncHandleTracer to wait for the log queue to be empty (after running `f`) before returning

## 0.1.1.0 -- 2021-6-2

* Print timestamp in log messages

## 0.1.0.0 -- 2021-5-4

* First version. Released on an unsuspecting world.