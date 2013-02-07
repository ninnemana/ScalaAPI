package com.curt.api

import com.twitter.finatra._
import com.twitter.finatra.ContentType._
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.http._
import com.twitter.finagle.http.{Request, Response}
import scala.util.DynamicVariable
import org.squeryl.{Session, SessionFactory}
import com.curt.vehicle._
import com.curt.database._
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
  
	class Api extends Controller with Database {

		/**
		 * Basic Example
		 *
		 * curl http://localhost:7070/hello => "hello world"
		 */
		get("/") { request =>
			render.plain("hello world").toFuture
		}

		get("/vehicle"){ request =>
			val system = ActorSystem("VehicleSystem")
			val vehicleActor = system.actorOf(Props[Vehicle], name = "vehicleActor")

			implicit val timeout = Timeout(5 seconds)
			val future = vehicleActor ? "years"
			val years = Await.result(future, timeout.duration).asInstanceOf[List[Int]]
			respondTo(request){
				case _:Json => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", "Years"),
									("Options", years))),
							("ProductMatch",
								Map(
									("Parts", ""),
									("Groups",""))))).toFuture
				}
				case _:All => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", "Years"),
									("Options", years))),
							("ProductMatch",
								Map(
									("Parts", ""),
									("Groups",""))))).toFuture
				}
			}
		}

		get("/vehicle/:year"){ request =>
			val year = request.routeParams.getOrElse("year","0").toInt
			val system = ActorSystem("VehicleSystem")
			val vehicleActor = system.actorOf(Props(new Vehicle(year)), name = "vehicleActor")

			implicit val timeout = Timeout(5 seconds)
			val future = vehicleActor ? "makes"
			val makes = Await.result(future,timeout.duration).asInstanceOf[List[String]]
			respondTo(request){
				case _:Json => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", "Makes"),
									("Options", makes))),
							("ProductMatch",
								Map(
									("Parts", ""),
									("Groups",""))))).toFuture
				}
				case _:All => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", "Makes"),
									("Options", makes))),
							("ProductMatch",
								Map(
									("Parts", ""),
									("Groups",""))))).toFuture
				}
			}
		}

		get("/vehicle/:year/:make"){ request =>
			val year = request.routeParams.getOrElse("year","0").toInt
			val make = request.routeParams.getOrElse("make","")
			
			val system = ActorSystem("VehicleSystem")
			val vehicleActor = system.actorOf(Props(new Vehicle(year,make)), name = "vehicleActor")

			implicit val timeout = Timeout(5 seconds)
			val future = vehicleActor ? "models"
			val models = Await.result(future,timeout.duration).asInstanceOf[List[String]]

			respondTo(request){
				case _:Json => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", "Models"),
									("Options", models))),
							("ProductMatch",
								Map(
									("Parts", ""),
									("Groups",""))))).toFuture
				}
				case _:All => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", "Models"),
									("Options", models))),
							("ProductMatch",
								Map(
									("Parts", ""),
									("Groups",""))))).toFuture
				}
			}
		}

		get("/vehicle/:year/:make/:model"){ request =>
			val year = request.routeParams.getOrElse("year","0").toInt
			val make = request.routeParams.getOrElse("make","")
			val model = request.routeParams.getOrElse("model","")

			val system = ActorSystem("VehicleSystem")
			val vehicleActor = system.actorOf(Props(new Vehicle(year,make,model)), name = "vehicleActor")

			implicit val timeout = Timeout(5 seconds)
			val future = vehicleActor ? "submodels"
			val submodels = Await.result(future,timeout.duration).asInstanceOf[List[String]]
			
			respondTo(request){
				case _:Json => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", "Submodels"),
									("Options", submodels))),
							("ProductMatch",
								Map(
									("Parts", ""),
									("Groups",""))))).toFuture
				}
				case _:All => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", "Submodels"),
									("Options", submodels))),
							("ProductMatch",
								Map(
									("Parts", ""),
									("Groups",""))))).toFuture
				}
			}
		}

		get("/vehicle/:year/:make/:model/:submodel"){ request =>
			val year = request.routeParams.getOrElse("year","0").toInt
			val make = request.routeParams.getOrElse("make","")
			val model = request.routeParams.getOrElse("model","")
			val submodel = request.routeParams.getOrElse("submodel","")
			
			val system = ActorSystem("VehicleSystem")
			val vehicleActor = system.actorOf(Props(new Vehicle(year,make,model,submodel)), name = "vehicleActor")

			implicit val timeout = Timeout(5 seconds)
			val future = vehicleActor ? "config"
			val options = Await.result(future,timeout.duration).asInstanceOf[Tuple2[String,List[String]]]

			respondTo(request){
				case _:Json => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", options._1),
									("Options", options._2))),
							("ProductMatch",
								Map(
									("Parts", ""),
									("Groups",""))))).toFuture
				}
				case _:All => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", options._1),
									("Options", options._2))),
							("ProductMatch",
								Map(
									("Parts", ""),
									("Groups",""))))).toFuture
				}
			}
		}

		get("/vehicle/:year/:make/:model/:submodel/*"){ request =>
			val year = request.routeParams.getOrElse("year","0").toInt
			val make = request.routeParams.getOrElse("make","")
			val model = request.routeParams.getOrElse("model","")
			val submodel = request.routeParams.getOrElse("submodel","")
			val encoded_config = request.routeParams.getOrElse("splat","")
			
			val config = URLDecoder.decode(encoded_config).split("/").toList

			println(config)
			val system = ActorSystem("VehicleSystem")
			val vehicleActor = system.actorOf(Props(new Vehicle(year,make,model,submodel,config)), name = "vehicleActor")

			implicit val timeout = Timeout(5 seconds)
			val future = vehicleActor ? "config"
			val options = Await.result(future,timeout.duration).asInstanceOf[Tuple2[String,List[String]]]

			respondTo(request){
				case _:Json => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", options._1),
									("Options", options._2))),
							("ProductMatch",
								Map(
									("Parts", ""),
									("Groups",""))))).toFuture
				}
				case _:All => {
					render.json(Map(
							("ConfigOption", 
								Map(
									("Type", options._1),
									("Options", options._2))),
							("ProductMatch",
								Map(
									("Parts", ""),
									("Groups",""))))).toFuture
				}
			}
		}


		/**
		 * Route parameters
		 *
		 * curl http://localhost:7070/user/dave => "hello dave"
		 */
		get("/user/:username") { request =>
			val username = request.routeParams.getOrElse("username", "default_user")
			render.plain("hello " + username).toFuture
		}

		/**
		 * Setting Headers
		 *
		 * curl -I http://localhost:7070/headers => "Foo:Bar"
		 */
		get("/headers") { request =>
			render.plain("look at headers").header("Foo", "Bar").toFuture
		}

		/**
		 * Rendering json
		 *
		 * curl -I http://localhost:7070/headers => "Foo:Bar"
		 */
		get("/data.json") { request =>
			render.json(Map("foo" -> "bar")).toFuture
		}

		/**
		 * Query params
		 *
		 * curl http://localhost:7070/search?q=foo => "no results for foo"
		 */
		get("/search") { request =>
			request.params.get("q") match {
				case Some(q) => render.plain("no results for "+ q).toFuture
				case None    => render.plain("query param q needed").status(500).toFuture
			}
		}

		/**
		 * Uploading files
		 *
		 * curl -F avatar=@/path/to/img http://localhost:7070/profile
		 */
		post("/profile") { request =>
			request.multiParams.get("avatar").map { avatar =>
				println("content type is " + avatar.contentType)
				avatar.writeToFile("/tmp/avatar") //writes uploaded avatar to /tmp/avatar
			}
			render.plain("ok").toFuture
		}

		/**
		 * Rendering views
		 *
		 * curl http://localhost:7070/posts
		 */
		class AnView extends View {
			val template = "an_view.mustache"
			val some_val = "random value here"
		}

		get("/template") { request =>
			val anView = new AnView
			render.view(anView).toFuture
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
			println(request.error)
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


		/**
		 * Dispatch based on Content-Type
		 *
		 * curl http://localhost:7070/index.json
		 * curl http://localhost:7070/index.html
		 */
		get("/blog/index.:format") { request =>
			respondTo(request) {
				case _:Html => render.html("<h1>Hello</h1>").toFuture
				case _:Json => render.json(Map("value" -> "hello")).toFuture
			}
		}

		/**
		 * Also works without :format route using browser Accept header
		 *
		 * curl -H "Accept: text/html" http://localhost:7070/another/page
		 * curl -H "Accept: application/json" http://localhost:7070/another/page
		 * curl -H "Accept: foo/bar" http://localhost:7070/another/page
		 */

		get("/another/page") { request =>
			respondTo(request) {
				case _:Html => render.plain("an html response").toFuture
				case _:Json => render.plain("an json response").toFuture
				case _:All => render.plain("default fallback response").toFuture
			}
		}
	}

	val dbSession = new DatabaseSessionService
	val app = new Api

	def main(args: Array[String]) = {
		FinatraServer.register(app)
		FinatraServer.addFilter(dbSession)
		FinatraServer.start()
	}
}