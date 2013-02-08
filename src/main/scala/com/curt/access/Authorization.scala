package com.curt.access

import org.squeryl.{KeyedEntity, SessionFactory, Session, Query,Queryable, Optimistic, Schema}
import org.squeryl.annotations.Column
import com.curt.database.Guid
import org.scala_tools.time.Imports._
import org.squeryl.PrimitiveTypeMode._
import com.curt.database.Database
import java.util.UUID

class Authorization(val key:Option[String]) extends Database {
  
  key match {
    case None => throw new Exception
    case Some(key) => {
      import CurtDev._
      transaction{
          val uKey = UUID.fromString(key)
          val result = join(apiKeys, apiKeyTypes, customerUsers)((ak,akt,cu) =>
            where(ak.api_key === uKey.toString)
            select((ak.id))
            on((ak.type_id === akt.id),
              (ak.user_id === cu.id))).toList

          if(result.length == 0){
            throw new Exception
          }
      }
    }
    
  }
}

class ApiKeys(val id:String, val api_key:String, val type_id:String, val user_id:String, val date_added:DateTime)
class ApiKeyTypes(val id:String, val key_type:String, val date_added:DateTime)
class CustomerUsers(val id:String, val name:String, val email:String, val password:String, val customerID:Int, val date_added:DateTime, val active:Boolean, val locationID:Int, val isSudo:Boolean, val cust_ID:Int, val NotCustomer:Boolean)

object CurtDev extends Schema {
	val apiKeys = table[ApiKeys]("ApiKey")
	val apiKeyTypes = table[ApiKeyTypes]("ApiKeyType")
	val customerUsers = table[CustomerUsers]("CustomerUser")
	
  on(apiKeyTypes)(t => declare(
    t.key_type is(named("key_type"))
    ))
}