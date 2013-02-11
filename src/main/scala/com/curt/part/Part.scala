package com.curt.part

import org.scala_tools.time.Imports.DateTime
import com.curt.vehicle._
import akka.actor._
import akka.dispatch.Await
import akka.dispatch.Future
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._
import java.net.URI
import org.squeryl.{Schema, KeyedEntity, SessionFactory, Session, Query,Queryable, Optimistic}
import org.squeryl.annotations.Column
import org.squeryl.PrimitiveTypeMode._
import com.curt.database.Database
import scala.collection.mutable.ListBuffer

class VehiclePart(year:Int,make:String,model:String,submodel:String,config:List[String]) extends Part {
	var VehicleAttributes:List[VehicleAttribute]	= 	List[VehicleAttribute]()
	var InstallTime:Double							=	0
	var	Drilling:String								=	""
	var	Exposed:String								=	""
	
	def this(y:Int,ma:String,mo:String) = this(y,ma,mo,"",List[String]())
	def this(y:Int,ma:String,mo:String,sm:String) = this(y,ma,mo,sm,List[String]())
	
	override def receive = {
	  case "base" => {
	    
	    import CurtDev._
		transaction{
			val results = join(vehicleParts,vehicles,baseVehicles,makes,models)((vp,v,bv,ma,mo) =>
							where(bv.YearID === year and ma.MakeName === make and mo.ModelName === model)
							select((vp.PartNumber))
							orderBy(vp.PartNumber asc)
							on((vp.VehicleID === v.ID),
								(v.BaseVehicleID === bv.ID),
								(bv.MakeID === ma.ID),
								(bv.ModelID === mo.ID))).distinct
			
			sender ! results.toList
		}
	  }
	  case "sub" => {
	    import CurtDev._
		transaction{
								
			val results = join(vehicleParts,vehicles,baseVehicles,submodels,makes,models)((vp,v,bv,sm,ma,mo) =>
							where(bv.YearID === year and ma.MakeName === make and mo.ModelName === model and sm.SubmodelName === submodel)
							select((vp.PartNumber))
							orderBy(vp.PartNumber asc)
							on((vp.VehicleID === v.ID),
								(v.BaseVehicleID === bv.ID),
								(v.SubModelID === sm.ID),
								(bv.MakeID === ma.ID),
								(bv.ModelID === mo.ID))).toIndexedSeq
			
			sender ! results.toList
		}
	  }
	  case "config" => {
	    import CurtDev._
		transaction{

			val results = join(vehicleParts,vehicles,vehicleConfigAttributes,configAttributes,baseVehicles,submodels,makes,models)((vp,v,vca,ca,bv,sm,ma,mo) =>
							where(bv.YearID === year and ma.MakeName === make and mo.ModelName === model and sm.SubmodelName === submodel and (ca.value in config))
							select((vp.PartNumber))
							orderBy(vp.PartNumber asc)
							on((vp.VehicleID === v.ID),
							    (vca.VehicleConfigID === v.ConfigID),
								(ca.ID === vca.AttributeID),
								(v.BaseVehicleID === bv.ID),
								(v.SubModelID === sm.ID),
								(bv.MakeID === ma.ID),
								(bv.ModelID === mo.ID))).toIndexedSeq
			
			sender ! results.toList
		}
	  }
	  
	  case _ => {
	    sender ! List[Int]()
	  }
	}
}

class Part extends Actor{
	var PartId:Int 						= 	0
	var CustPartId:Int 					= 	0
	var Status:Int 						= 	0
	var DateModified:DateTime 			= 	DateTime.now
	var DateAdded:DateTime 				= 	DateTime.now
	var AverageReview:Int 				= 	0
	var ShortDesc:String 				= 	""
	var PartClass:String				=	""
	var RelatedCount:Int				=	0
	var PriceCode:Int					=	0
	var InstallitionSheet:URI			=	URI.create("")
	    
	var Attributes:List[PartAttribute] 	= 	List[PartAttribute]()
	var Content:List[PartContent]		=	List[PartContent]()
	var Pricing:List[Pricing] 			= 	List[Pricing]()
	var Reviews:List[PartReview]		=	List[PartReview]()
	var Images:List[PartImage]			=	List[PartImage]()
  
	def receive = {
		case id:Int => {
			this.PartId = id
			println(this)
			sender ! this
		}
		case "part_attributes" => {
			println("we're gonna load some part attributes")
			println(this.PartId)
			sender ! this.Attributes
		}
		case "vehicles" => {
			println("we're gonna load the vehicles that match this part")
		}
		case _ => new Part
	}
}

class PartAttribute(k:String, v:String) {
	def this() 			= 	this("","")
	def this(t:String) 	= 	this(t,"")
	  
	var Key:String 		= 	""
	var Value:String 	= 	""
}

class PartContent(k:String, v:String) {
	def this() 			= 	this("","")
	def this(t:String) 	= 	this(t,"")
	  
	var Key:String 		= 	""
	var Value:String 	= 	""
}

class Pricing(t:String, p:Double){
	def this() 			= 	this("",0)
	def this(p:Int) 	= 	this("",p)
	
	var Type:String 	= 	""
	var Price:String 	= 	""
}

class PartImage {
	var Size:String 	= 	""
	var Path:URI	 	= 	URI.create("")
	var Height:Double	=	0
	var Width:Double	=	0
	var Sort:String 	=	""
}

class PartReview {
	var Rating:Double			=	0
	var Subject:String			=	""
	var Review:String			=	""
	var Name:String				=	""
	var Email:String			=	""
	var	CreatedDate:DateTime	=	DateTime.now
}

class PartVideo {
	var YouTubeVideoId:String	=	""
	var IsPrimary:Boolean		=	false
	var Type:String				=	""
	var TypeIcon:URI			=	URI.create("")
}
