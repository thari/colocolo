package org.guigna.uber


import kodkod.ast.{Formula, Relation, Variable}
import kodkod.instance.{Bounds, Universe}

import scala.collection.JavaConverters._
/**
  * Created by hariharan on 6/15/16.
  */
class Uber {

  // abstract sig UberVehicle {}
  private val Vehicle = Relation.unary("Vehicle")

  // There are two types of UberVehicles
  // UberX represents small vehicles, SUV represents large vehicles
  //sig UberX, SUV extends UberVehicle {}
  private val UberX = Relation.unary("UberX")
  private val SUV = Relation.unary("SUV")

  //abstract sig Passenger{}
  private val Passenger = Relation.unary("Passenger")

  // There are two types of passengers: single or group
  //sig SinglePassenger, GroupPassenger extends Passenger{}
  private val SinglePassenger = Relation.unary("SinglePassenger")
  private val GroupPassenger = Relation.unary("GroupPassenger")

  // Blocks represent abstract locations
  //sig Block {}
  private val Block = Relation.unary("Block")

  //one sig Control {
  //TODO: yet to figure out how to represent one
  private val Control = Relation.unary("Control")

  private val uberLoc = Relation.ternary("UberLoc")

  private val passengerLoc = Relation.ternary("passengerLoc")

  private val ride = Relation.ternary("ride")

  def declarations() : Formula = {
    val u1 = Vehicle eq(UberX union SUV)
    val u2 = Passenger eq(SinglePassenger union GroupPassenger)
    val f1 = uberLoc.in(Control.product(Vehicle).product(Block))
    val f2 = passengerLoc.in(Control.product(Passenger).product(Block))
    val f3 = ride.in(Control.product(Vehicle).product(Passenger))
    u1 and u2 and f1 and f2 and f3
  }

  def req0() : Formula = {
    val v = Variable.unary("v")
    val v0 = (v in ((Control join uberLoc) join Block)) and (v in ((Control join ride) join Passenger))
    val v1 = v0.forAll(v.oneOf(Vehicle))

    val b  = Variable.unary("b")
    val b0 = (b in (Vehicle join (Control join uberLoc))) and (b in (Passenger join (Control join passengerLoc)))
    val b1 = b0.forAll(b.oneOf(Block))

    val p = Variable.unary("p")
    val p0 = (p in ((Control join passengerLoc) join Block)) and (p in (Vehicle join (Control join ride)))
    val p1 = p0.forAll(p.oneOf(Passenger))

    v1 and b1 and p1
  }

  //Vehicle can be only in one location at any point of time
  def req1() : Formula = {
    val c = Variable.unary("c")
    (c join (Control join uberLoc)).one().forAll(c.oneOf(Vehicle))
  }

  //a passenger can travel only in atmost one car at any point of time
  def req2() : Formula = {
    val p = Variable.unary("p")
    (p join (Control join ride).transpose()).lone().forAll(p.oneOf(Passenger))
  }

  def req3() : Formula = {
    val ux = Variable.unary("UberX")
    ((ux join (Control join ride)) in SinglePassenger).forAll(ux.oneOf(UberX))
  }

  def req4() : Formula = {
    val ux = Variable.unary("UberX")
    (ux join (Control join ride)).lone().forAll(ux.oneOf(UberX))
  }

  def req5() : Formula = {
    val p = Variable.unary("p")
    (p join (Control join passengerLoc)).one().forAll(p.oneOf(Passenger))
  }

//  def req6() : Formula = {
//    val u  = Variable.unary("u")
//    val p = Variable.unary("p")
//    ((u join (Control join ride)).eq(p)).iff((u join (Control join uberLoc)).eq(p join (Control join passengerLoc)))
//  }

  def show(): Formula = {
    req0() and req1() and req2() and req3() and req4() and req5()
  }



  def bounds(uberx : Int, suv : Int, singleP : Int, groupP : Int, block : Int) : Bounds = {
    var atoms = scala.collection.immutable.List.empty[String]

    for (a <- 0 to uberx){
      atoms = atoms :+ ("UberX" + a.toString)
    }

    for (a <- 0 to suv){
      atoms = atoms :+ ("SUV" + a.toString)
    }

    for (a <- 0 to singleP){
      atoms = atoms :+ ("SinglePassenger" + a.toString)
    }

    for (a <- 0 to groupP){
      atoms = atoms :+ ("GroupPassenger" + a.toString)
    }

    for (a <- 0 to block){
      atoms = atoms :+ ("Block" + a.toString)
    }

    atoms = atoms :+ "Control"

    val u = new Universe(atoms.asJava)

    val f = u.factory()

    val b = new Bounds(u)

    val uberxB = f.range(f.tuple("UberX0"), f.tuple("UberX"+(uberx-1)))
    val suvB = f.range(f.tuple("SUV0"), f.tuple("SUV"+(uberx-1)))
    val spB = f.range(f.tuple("SinglePassenger0"), f.tuple("SinglePassenger"+(uberx-1)))
    val gpB = f.range(f.tuple("GroupPassenger0"), f.tuple("GroupPassenger"+(uberx-1)))
    val blockB = f.range(f.tuple("Block0"), f.tuple("Block"+(uberx-1)))
    val cB = f.setOf("Control")
    val vB = uberxB.clone()
    vB.addAll(suvB)
    val pB = spB.clone()
    pB.addAll(gpB)

    b.boundExactly(Control, cB)
    b.bound(UberX,uberxB)
    b.bound(SUV, suvB)
    b.bound(SinglePassenger, spB)
    b.bound(GroupPassenger,gpB)
    b.boundExactly(Block, blockB)
    b.bound(Vehicle, vB)
    b.bound(Passenger, pB)
    b.bound(uberLoc, cB.product(vB).product(blockB))
    b.bound(passengerLoc, cB.product(pB).product(blockB))
    b.bound(ride, cB.product(vB).product(pB))
    b
  }

  def bounds(all : Int) : Bounds = {
    bounds(all,all,all,all,all)
  }
}
