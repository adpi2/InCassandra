
/**
  * Created by Adrien on 1/18/2018.
  */
abstract class Keyspace(name: String) {}
abstract class Table(keyspace: Keyspace, name: String) {}
abstract class Column[TTable <: Table, TType](name: String) {}

object MyKeyspace extends Keyspace("my_keyspace") {}
object Product extends Table(MyKeyspace, "product") {}
object ProductName extends Column[Product.type, String]("product_name") {}

case class From[TTable <: Table] private (table: TTable) {
  def select[TType](column : Column[TTable, TType]): Select[TTable, TType] =
    new Select[TTable, TType](table, Set[Column[TTable, _]](column))
}

object Cql {
  def from[TTable <: Table](table: TTable) : From[TTable] = new From[TTable](table)

  implicit class RichColumn[TTable, TType](self: Column[TTable, TType]) {
    def ==(value: TType): Relation[TTable] = new Relation[TTable]()
    def <(value: TType): Relation[TTable] = new Relation[TTable]()
    def <=(value: TType): Relation[TTable] = new Relation[TTable]()
    def >(value: TType): Relation[TTable] = new Relation[TTable]()
    def >=(value: TType): Relation[TTable] = new Relation[TTable]()
  }
}

class Relation[TTable <: Table] {}

trait Cassandra {}


case class SelectStatement[A](request: Cassandra => Stream[A]){
  import SelectStatement._

  def flatMap[B](f: A => SelectStatement[B]) = SelectStatement(c => request(c).flatMap(a => f(a).request(c)))
  def map[B](f: A => B): SelectStatement[B] = flatMap(a => unit(f(a)))
  def map2[B, C](sb: SelectStatement[B])(f: (A, B) => C): SelectStatement[C] = flatMap(a => sb.map(b => f(a, b)))
}

object SelectStatement {
  def unit[A](a: A): SelectStatement[A] = new SelectStatement[A](_ => Stream(a))
}

class Select[TTable <: Table, A] private (table: TTable, columns: Set[Column[TTable, _]]) {
  def where(relation: Relation[TTable]): FilteredSelect[TTable, A] = new FilteredSelect[TTable, A](this, relation)
}

class FilteredSelect[TTable <: Table, A](select: Select[TTable, A], relation: Relation[TTable]) extends Select[TTable, A] {
  def and(relation: Relation[TTable]): FilteredSelect[TTable, A] = new FilteredSelect[TTable, A](this, relation)
}

