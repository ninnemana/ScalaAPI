CURT Scala API
=========
---------

> The new version of the CURT API used the [Finagle Server](http://twitter.github.com/finagle) built by Twitter
for asynchronous RPC processing. Some of the best features a listed below:

  - Concurrent JDBC using [Akka Actors](http://doc.akka.io/docs/akka/snapshot/scala/actors.html)
  - JSON rendering powered by [Jerkson](https://github.com/codahale/jerkson)
  - ACES Compliant vehicle lookup with product groups
 

--------
Endpoints
---------
---------

#### Vehicle

---

*Get Years*

    GET - http://api.curtmfg.com/v3/vehicle?key=[public api key]

*Get Makes*

    GET - http://api.curtmfg.com/v3/vehicle/2012?key=[public api key]

*Get Models*

    GET - http://api.curtmfg.com/v3/vehicle/2012/Audi?key=[public api key]

*Get SubModels*

    GET - http://api.curtmfg.com/v3/vehicle/2012/Audi/A5?key=[public api key]

*Get Dynamic Configuration Option*

    GET - http://api.curtmfg.com/v3/vehicle/2012/Audi/A5/Cabriolet?key=[public api key]

*Get Next Dynamic Configuration Option*

    GET - http://api.curtmfg.com/v3/vehicle/2012/Audi/A5/Cabriolet/Coupe?key=[public api key]


----



Philoshopy
-

> This version if the API is meant to focus on data quantity while maintaining, if not improving performance, by leveraging concurrency. We would like the client to have the ability to make fewer requests to the API Server and be provided with a larger amount of data in the response.

Contributors
-
* Alex Ninneman
    * [Github](http://github.com/ninnemana)
    * [Twitter](https://twitter.com/ninnemana)
* Jessica Janiuk
    * [Github](http://github.com/janiukjf)
    * [Twitter](http://twitter.com/janiukjf)



License
-

MIT

*Free Software, Fuck Yeah!*
  
    