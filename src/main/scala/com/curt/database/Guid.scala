package com.curt.database

import java.util.UUID
abstract class TypeSafeId[I, T](val id: I) extends Serializable
class Guid[T](override val id: String) extends TypeSafeId[String, T](id)
class UserGuid(override val id: String) extends Guid[Any](id)

trait GuidFactory[G] {
  def apply(id: String): G
  def apply(id: UUID): G = apply(id.toString)
  def apply(ms: Long, ls: Long): G = apply(new UUID(ms, ls))
  def apply(bytes: Array[Byte]): G = apply(UUID.nameUUIDFromBytes(bytes))
  def random = apply(UUID.randomUUID())
}

object Guid {
  def apply[T] = new GuidFactory[Guid[T]] {
    def apply(id: String) = new Guid[T](id)
  }
}

object UserGuid extends GuidFactory[UserGuid] {
  override def apply(id: String) = new UserGuid(id)
}