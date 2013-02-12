package com.curt.api

import com.twitter.finatra._
import com.twitter.finatra.ContentType._
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.http._
import com.twitter.finagle.http.{Request, Response}
//import com.twitter.finagle.http.filter.JsonpFilter
import com.twitter.finagle.http.filter._
import com.twitter.util.Future
import scala.util.DynamicVariable
import org.squeryl.{Session, SessionFactory}
import com.curt.vehicle._
import com.curt.database._
import com.curt.access.Authorization
import akka.actor._
import akka.dispatch.Await
import akka.dispatch.Future
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._
import akka.japi.Creator
import text.Document
import java.net.URLDecoder

object App {


	class DatabaseSessionService extends SimpleFilter[Request, Response] {
		val dbSession = new DynamicVariable[Session](null)
		def apply(request: Request, continue: Service[Request, Response]) = {
			dbSession.withValue(SessionFactory.newSession) {
				dbSession.value.bindToCurrentThread
				try {
					continue(request)
				} finally {
					dbSession.value.close
					dbSession.value.unbindFromCurrentThread
				}
			}
		}
	}
	
	/**
	 * A simple Filter that checks that the request is valid by inspecting the
	 * "Authorization" header.
	 */
	class Authorize extends SimpleFilter[Request, Response] {
		def apply(request: Request, continue: Service[Request, Response]) = {
			try{
				// Try and retrieve an API from the Query String
				var key = Option(request.getParam("key", ""))
				
                key match {
				  case None => {
					  var auth_key = request.getHeader("Authorization")
					  new Authorization(Option(auth_key))
				  }
				  case Some(key) => {
					  new Authorization(Option(key))
				  }
                }
				continue(request)
				
			} catch {
			  case _ => {
				  	val req = com.twitter.finagle.http.Request
					continue(req.apply("/unauthorized"))
			  }
			}
		}
	}
	
	class JsonpFilter[REQUEST <: Request] extends SimpleFilter[REQUEST, Response] {
	  def apply(request: REQUEST, service: Service[REQUEST, Response]): com.twitter.util.Future[Response] = {
	    service(request) onSuccess { response =>
	      if(response.mediaType == Some(MediaType.Json)) {
	        response.content = response.getContent
	        response.mediaType = MediaType.Javascript
	      }
	    }
	  }
	}
  
	
	/*
	 * The routing for the API is documented in the FINATRA.markdown file
	 */
	class Api extends Controller with Database {

		/*
		 * Site Root
		 */
		get("/") { request =>
			render.plain("hello world").toFuture
		}

		/*
		 * Return a list of years with an empty Array for
		 * Parts and Groups (we're not going to find any for this)
		 * curl http://localhost:7070/vehicle?key=[public key]
		 * 
		 * Method: GET
		 * @param key (required) - Public API Key
		 */
		get("/vehicle"){ request =>
			val system = ActorSystem("VehicleSystem")
			val vehicleActor = system.actorOf(Props[Vehicle], name = "vehicleActor")

			implicit val timeout = Timeout(5 seconds)
			val future = vehicleActor ? "years"
			val years = Await.result(future, timeout.duration).asInstanceOf[List[Int]]
			
			val parts = List[Int]()
			val groups = List[Int]()
			respondTo(request){
				case _:Json => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", "Years"),
									("Options", years))),
							("ProductMatch",
								Map(
									("Parts", parts),
									("Groups", groups))))).toFuture
				}
				case _:All => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", "Years"),
									("Options", years))),
							("ProductMatch",
								Map(
									("Parts", parts),
									("Groups", groups))))).toFuture
				}
			}
		}

		
		/*
		 * Return a list of makes with an empty Array for
		 * Parts and Groups (we're not going to find any for this)
		 * curl http://localhost:7070/vehicle/2012?key=[public key]
		 * 
		 * Method: GET
		 * @param :year (required) - Year to retrieve makes for
		 * @param key (required) - Public API Key
		 */
		get("/vehicle/:year"){ request =>
			val year = request.routeParams.getOrElse("year","0").toInt
			val system = ActorSystem("VehicleSystem")
			val vehicleActor = system.actorOf(Props(new Vehicle(year)), name = "vehicleActor")

			implicit val timeout = Timeout(5 seconds)
			val future = vehicleActor ? "makes"
			val makes = Await.result(future,timeout.duration).asInstanceOf[List[String]]
			
			val parts = List[Int]()
			val groups = List[Int]()
			respondTo(request){
				case _:Json => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", "Makes"),
									("Options", makes))),
							("ProductMatch",
								Map(
									("Parts", parts),
									("Groups", groups))))).toFuture
				}
				case _:All => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", "Makes"),
									("Options", makes))),
							("ProductMatch",
								Map(
									("Parts", parts),
									("Groups", groups))))).toFuture
				}
			}
		}

		/*
		 * Return a list of models with an array of part IDs
		 * and group IDs for this configuration.
		 * curl http://localhost:7070/vehicle/2012/Audi?key=[public key]
		 * 
		 * Method: GET
		 * @param :year (required) - Year to retrieve models for
		 * @param :make (required) - Make to retrieve models for
		 * @param key (required) - Public API Key
		 */
		get("/vehicle/:year/:make"){ request =>
			val year = request.routeParams.getOrElse("year","0").toInt
			val make = URLDecoder.decode(request.routeParams.getOrElse("make",""))
			
			val system = ActorSystem("VehicleSystem")
			val vehicleActor = system.actorOf(Props(new Vehicle(year,make)), name = "vehicleActor")

			implicit val timeout = Timeout(5 seconds)
			val future = vehicleActor ? "models"
			val models = Await.result(future,timeout.duration).asInstanceOf[List[String]]

			val parts = List[Int]()
			val groups = List[Int]()
			respondTo(request){
				case _:Json => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", "Models"),
									("Options", models))),
							("ProductMatch",
								Map(
									("Parts", parts),
									("Groups", groups))))).toFuture
				}
				case _:All => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", "Models"),
									("Options", models))),
							("ProductMatch",
								Map(
									("Parts", parts),
									("Groups", groups))))).toFuture
				}
			}
		}

		/*
		 * Return a list of sub-models with an array of part IDs
		 * and group IDs for this configuration.
		 * curl http://localhost:7070/vehicle/2012/Audi/A5?key=[public key]
		 * 
		 * Method: GET
		 * @param :year (required) - Year to retrieve sub-models for
		 * @param :make (required) - Make to retrieve sub-models for
		 * @param :model (required) - Model to retrieve sub-models for
		 * @param key (required) - Public API Key
		 */
		get("/vehicle/:year/:make/:model"){ request =>
			val year = request.routeParams.getOrElse("year","0").toInt
			val make = URLDecoder.decode(request.routeParams.getOrElse("make",""))
			val model =  URLDecoder.decode(request.routeParams.getOrElse("model",""))
			
			val system = ActorSystem("VehicleSystem")
			implicit val timeout = Timeout(5 seconds)
			
			// Create our actor for getting the submodels, parts, and groups
			val subActor = system.actorOf(Props(new Vehicle(year,make,model)), name = "subActor")
			val partActor = system.actorOf(Props(new Vehicle(year,make,model)), name = "partActor")
			val groupActor = system.actorOf(Props(new Vehicle(year,make,model)), name = "groupActor")
			
			// Curtains up for our actors
			val subFuture = subActor ? "submodels"
			val partFuture = partActor ? "parts"
			val groupFuture = groupActor ? "groups"
			
			// And we're impressed
			val submodels = Await.result(subFuture,timeout.duration).asInstanceOf[List[String]]
			val parts = Await.result(partFuture,timeout.duration).asInstanceOf[List[Int]]
			val groups = Await.result(groupFuture,timeout.duration).asInstanceOf[List[Int]]
			
			// Render our response based on "Accept" Header
			respondTo(request){
				case _:Json => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", "Submodels"),
									("Options", submodels))),
							("ProductMatch",
								Map(
									("Parts", parts),
									("Groups",groups))))).toFuture
				}
				case _:All => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", "Submodels"),
									("Options", submodels))),
							("ProductMatch",
								Map(
									("Parts", parts),
									("Groups", groups))))).toFuture
				}
			}
		}

		/*
		 * Return a list of configuration options with an array of part IDs
		 * and group IDs for this configuration.
		 * curl http://localhost:7070/vehicle/2012/Audi/A5/Cabriolet?key=[public key]
		 * 
		 * Method: GET
		 * @param :year (required) - Year to retrieve configuration options for
		 * @param :make (required) - Make to retrieve configuration options for
		 * @param :model (required) - Model to retrieve configuration options for
		 * @param :submodel (required) - Sub-Model to retrieve configuration options for
		 * @param key (required) - Public API Key
		 */
		get("/vehicle/:year/:make/:model/:submodel"){ request =>
			val year 		= 	request.routeParams.getOrElse("year","0").toInt
			val make 		=  	URLDecoder.decode(request.routeParams.getOrElse("make",""))
			val model 		=  	URLDecoder.decode(request.routeParams.getOrElse("model",""))
			val submodel 	=  	URLDecoder.decode(request.routeParams.getOrElse("submodel",""))
			
			val system = ActorSystem("VehicleSystem")
			implicit val timeout = Timeout(5 seconds)
			
			val configActor = system.actorOf(Props(new Vehicle(year,make,model,submodel)), name = "configActor")
			val partActor = system.actorOf(Props(new Vehicle(year,make,model,submodel)), name = "partActor")
			val groupActor = system.actorOf(Props(new Vehicle(year,make,model,submodel)), name = "groupActor")
			
				
			val configFuture = configActor ? "config"
			val partFuture = partActor ? "parts"
			val groupFuture = groupActor ? "groups"
			
			val options = Await.result(configFuture,timeout.duration).asInstanceOf[Tuple2[String,List[String]]]
			val parts = Await.result(partFuture,timeout.duration).asInstanceOf[List[Int]]
			val groups = Await.result(groupFuture,timeout.duration).asInstanceOf[List[Int]]
			respondTo(request){
				case _:Json => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", options._1),
									("Options", options._2))),
							("ProductMatch",
								Map(
									("Parts", parts),
									("Groups",groups))))).toFuture
				}
				case _:All => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", options._1),
									("Options", options._2))),
							("ProductMatch",
								Map(
									("Parts", parts),
									("Groups", groups))))).toFuture
				}
			}
		}

		/*
		 * Return a list of configuration options with an array of part IDs
		 * and group IDs for this configuration.
		 * curl http://localhost:7070/vehicle/2012/Audi/A5/Cabriolet/Coupe?key=[public key]
		 * 
		 * Method: GET
		 * @param :year (required) - Year to retrieve configuration options for
		 * @param :make (required) - Make to retrieve configuration options for
		 * @param :model (required) - Model to retrieve configuration options for
		 * @param :submodel (required) - Sub-Model to retrieve configuration options for
		 * @param * "splat" (required) - Selected configuration iptions to retrieve further configuration options for
		 * @param key (required) - Public API Key
		 */
		get("/vehicle/:year/:make/:model/:submodel/*"){ request =>
			val year 		= 	request.routeParams.getOrElse("year","0").toInt
			val make 		=  	URLDecoder.decode(request.routeParams.getOrElse("make",""))
			val model 		=  	URLDecoder.decode(request.routeParams.getOrElse("model",""))
			val submodel 	=  	URLDecoder.decode(request.routeParams.getOrElse("submodel",""))
			val config 		= 	URLDecoder.decode(request.routeParams.getOrElse("splat","")).split("/").toList

			val system = ActorSystem("VehicleSystem")
			implicit val timeout = Timeout(5 seconds)
			
			val configActor = system.actorOf(Props(new Vehicle(year,make,model,submodel, config)), name = "configActor")
			val partActor = system.actorOf(Props(new Vehicle(year,make,model,submodel, config)), name = "partActor")
			val groupActor = system.actorOf(Props(new Vehicle(year,make,model,submodel, config)), name = "groupActor")
			
				
			val configFuture = configActor ? "config"
			val partFuture = partActor ? "parts"
			val groupFuture = groupActor ? "groups"
			
			val options = Await.result(configFuture,timeout.duration).asInstanceOf[Tuple2[String,List[String]]]
			val parts = Await.result(partFuture,timeout.duration).asInstanceOf[List[Int]]
			val groups = Await.result(groupFuture,timeout.duration).asInstanceOf[List[Int]]
			respondTo(request){
				case _:Json => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", options._1),
									("Options", options._2))),
							("ProductMatch",
								Map(
									("Parts", parts),
									("Groups", groups))))).toFuture
				}
				case _:All => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", options._1),
									("Options", options._2))),
							("ProductMatch",
								Map(
									("Parts", parts),
									("Groups", groups))))).toFuture
				}
			}
		}


		


		/**
		 * Custom Error Handling
		 *
		 * curl http://localhost:7070/error
		 */
		get("/error")   { request =>
			1234/0
			render.plain("we never make it here").toFuture
		}

		/**
		 * Custom Error Handling with custom Exception
		 *
		 * curl http://localhost:7070/unautorized
		 */
		class Unauthorized extends Exception

		get("/unauthorized") { request =>
			throw new Unauthorized
		}

		error { request =>
			request.error match {
				case Some(e:ArithmeticException) =>
					render.status(500).plain("whoops, divide by zero!").toFuture
				case Some(e:Unauthorized) =>
					render.status(401).plain("Not Authorized!").toFuture
				case Some(e:UnsupportedMediaType) =>
					render.status(415).plain("Unsupported Media Type!").toFuture
				case _ =>
					render.status(500).plain("Something went wrong!").toFuture
			}
		}


		/**
		 * Custom 404s
		 *
		 * curl http://localhost:7070/notfound
		 */
		notFound { request =>
			render.status(404).plain("not found yo").toFuture
		}

	}

	val dbSession = new DatabaseSessionService
	val authorized = new Authorize
	val cors = new AddResponseHeadersFilter(Map(
	    "Access-Control-Allow-Origin"  -> "*",
	    "Access-Control-Allow-Methods" -> "GET",
	    "Access-Control-Allow-Headers" -> "*"))
	val app = new Api
	

	def main(args: Array[String]) = {
		FinatraServer.register(app)
		FinatraServer.addFilter(dbSession)
		FinatraServer.addFilter(authorized)
		FinatraServer.addFilter(cors)
		FinatraServer.start()
	}
}