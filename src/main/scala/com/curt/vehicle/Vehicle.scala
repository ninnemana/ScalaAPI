package com.curt.vehicle

import scala.collection.mutable.ListBuffer
import com.twitter.util.Future
import org.squeryl.{Schema, KeyedEntity, SessionFactory, Session, Query,Queryable, Optimistic}
import org.squeryl.annotations.Column
import org.squeryl.PrimitiveTypeMode._
import com.curt.database.Database
import akka.actor._
import akka.dispatch.Await
import akka.dispatch.Future
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._


// The root object of the schema. Inheriting KeyedEntity[T] is not
// mandatory it just makes primary key methods availabile(delete and lookup) on tables.
abstract class VehicleObject extends KeyedEntity[Int]{
	val ID: Int = 0
}

class Years(val YearID: Int) extends KeyedEntity[Int] {

	def id = YearID
	// IMPORTANT : currently classes with Option[] members *must* provide a zero arg
	// constructor where every Option[T] member gets initialized with Some(t:T).
	// or else Squeryl will not be able to reflect the type of the field, and an exception will
	// be thrown at table instantiation time.
	def this() = this(0)

	def all:List[Int] = {
		transaction {
			return from(CurtDev.years)(y => select (y.YearID)orderBy(y.YearID desc)).toList
		}
	}

	def get_makes:List[String] = {
		transaction {
			val results  = join(CurtDev.makes, CurtDev.baseVehicles, CurtDev.vehicles,CurtDev.vehicleParts)((m,bv,v,vp) =>
						where(bv.YearID === YearID)
						select((m.MakeName))
						on((m.ID === bv.MakeID),
						(bv.ID === v.BaseVehicleID),
						(v.ID === vp.VehicleID))).distinct
			results.toList	
		}
		
	}

}
class Makes(val ID: Int, val AAIAMakeID: Int, val MakeName: String){

	def this() = this(0,0,"")

	def all = {
		from(CurtDev.makes)(m => select(m.MakeName)orderBy(m.MakeName asc)).toList
	}

	def get_models(y:Int,m:String):List[String] = {
		transaction {
			val results = join(CurtDev.makes, CurtDev.baseVehicles, CurtDev.vehicles, CurtDev.vehicleParts, CurtDev.models)((ma, bv, v, vp, mo) =>
				where(bv.YearID === y and ma.MakeName === m)
				select((mo.ModelName))
				on((ma.ID === bv.MakeID),
					(bv.ID === v.BaseVehicleID),
					(v.ID === vp.VehicleID),
					(mo.ID === bv.ModelID))).distinct
			results.toList
		}
	}

}
class Models(val ID: Int, val AAIAModelID: Int, val ModelName: String, val VehicleTypeID: Int){
	def this() = this(0,0,"",0)

	def get_submodels(y:Int,make:String,model:String):List[String] = {
		transaction {
			val results = join(CurtDev.models, CurtDev.baseVehicles, CurtDev.makes, CurtDev.vehicles, CurtDev.vehicleParts, CurtDev.submodels)((mo,bv, ma, v, vp, sm) =>
					where(bv.YearID === y and ma.MakeName === make and mo.ModelName === model)
					select((sm.SubmodelName))
					on((mo.ID === bv.ModelID),
						(ma.ID === bv.MakeID),
						(bv.ID === v.BaseVehicleID),
						(v.ID === vp.VehicleID),
						(sm.ID === v.SubModelID))).distinct
			results.toList
		}
	}
}
class Submodels(val ID: Int, val AAIASubmodelID: Int, val SubmodelName: String){
	def this() = this(0,0,"")
}
class BaseVehicles(val ID: Int, val AAIABaseVehicleID: Int, val YearID: Int, val MakeID: Int, val ModelID: Int)
class Vehicles(val ID: Int, val BaseVehicleID: Int, val SubModelID: Int, val ConfigID: Int, val AppID: Int)
class VehicleParts(val ID: Int, val VehicleID: Int, val PartNumber: Int)
class ConfigAttributes(val ID: Int, val ConfigAttributeTypeID: Int, val parentID: Int,  val vcdbID: Int, val value: String)
class ConfigAttributeTypes(val ID: Int, val name: String, val AcesTypeID: Int, val sort: Int){
	def this() = this(0,"",0,0)

	def get(year:Int,make:String,model:String,submodel:String,config:List[String]) = {
		import CurtDev._

		transaction{

			// OK THE QUERY IS WORKING, NOT WE JUST NEED AN ORDER BY AND WE NEED
			// TO CREATE OUR MAP FROM THE DATA
			val results = join(configAttributeTypes,configAttributes,vehicleConfigAttributes,vehicles,baseVehicles,submodels,makes,models)((cat,ca,vca,v,bv,sm,ma,mo) =>
				where(bv.YearID === year and ma.MakeName === make and mo.ModelName === model and sm.SubmodelName === submodel and (cat.name notIn 
					(join(configAttributeTypes, configAttributes)((cat2, ca) =>
							where(ca.value in config)
							select((cat2.name))
							on(cat2.ID === ca.ConfigAttributeTypeID)))))
				select(cat.name, ca.value)
				orderBy(cat.sort asc)
				on((cat.ID === ca.ConfigAttributeTypeID),
					(ca.ID === vca.AttributeID),
					(vca.VehicleConfigID === v.ConfigID),
					(v.BaseVehicleID === bv.ID),
					(v.SubModelID === sm.ID),
					(bv.MakeID === ma.ID),
					(bv.ModelID === mo.ID))).toIndexedSeq
			
			var name = ""
			val values = ListBuffer[String]()
			for( result <- results) {
				if(name.length() == 0){
					name += result._1.trim
					values += result._2.trim
				}else if(name == result._1 && !values.contains(result._2.trim)){
					values += result._2.trim
				}
			}
			new Tuple2(name, values)
			//Map.empty[String,List[String]]
		}
	}
}
class VehicleConfigAttributes(val ID: Int, val AttributeID: Int, val VehicleConfigID: Int)
class VehicleConfigs(val ID: Int, val AAIAVehicleConfigID: Int)


class Vehicle(year:Int, make:String, model:String, submodel:String, config: List[String]) extends Actor{

	// Constructor Overrides
	def this() = this(0,"","","",List[String]())
	def this(y:Int) = this(y,"","","",List[String]())
	def this(year:Int,make:String) = this(year,make,"","",List[String]())
	def this(year:Int,make:String,model:String) = this(year,make,model,"",List[String]())
	def this(year:Int,make:String,model:String,submodel:String) = this(year,make,model,submodel,List[String]())

	// Implement receive for our Actor
	// calling sender ! val will return val to the caller.
	def receive = {
		case "years" => { 
			sender ! new Years().all
		}
		case "makes" => {
			sender ! new Years(year).get_makes
		}
		case "models" => {
			val makes = new Makes
			sender ! makes.get_models(year,make)
		}
		case "submodels" => {
			val models = new Models
			sender ! models.get_submodels(year,make,model)
		}
		case "config" => {
			val configAttr = new ConfigAttributeTypes
			sender ! configAttr.get(year,make,model,submodel,config)
		}
		case _ => println("shit we missed")
	}
}

object CurtDev extends Schema {
	val years = table[Years]("vcdb_Year")
	val makes = table[Makes]("vcdb_Make")
	val models = table[Models]("vcdb_Model")
	val submodels = table[Submodels]("Submodel")
	val baseVehicles = table[BaseVehicles]("BaseVehicle")
	val vehicles = table[Vehicles]("vcdb_Vehicle")
	val vehicleParts = table[VehicleParts]("vcdb_VehiclePart")
	val configAttributes = table[ConfigAttributes]("ConfigAttribute")
	val configAttributeTypes = table[ConfigAttributeTypes]("ConfigAttributeType")
	val vehicleConfigAttributes = table[VehicleConfigAttributes]("VehicleConfigAttribute")
	val vehicleConfigs = table[VehicleConfigs]("VehicleConfig")
}