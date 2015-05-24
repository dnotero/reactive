/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = { 
    case op: Operation => root ! op

    case GC => 
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context become garbageCollecting(newRoot)
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case GC =>
    
    case op: Operation => 
      pendingQueue = pendingQueue.enqueue(op)
    
    case CopyFinished =>
      processPendingOperations(newRoot)
      changeRoot(newRoot)
      context become normal
  }

  private def processPendingOperations(newRoot: ActorRef): Unit =
    while(!pendingQueue.isEmpty) {
      newRoot ! pendingQueue.head
      pendingQueue = pendingQueue.tail
    }

  private def changeRoot(newRoot: ActorRef): Unit = {
    root ! PoisonPill
    root = newRoot
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = { 
    case Insert(requester, id, element) =>
      if(elem == element) process(requester, id, false)
      else getChild(element) ! Insert(requester, id, element)

    case Remove(requester, id, element) => 
      if(elem == element) process(requester, id, true)
      else getChild(element) ! Remove(requester, id, element)

    case Contains(requester, id, element) => 
      if(element == elem) requester ! ContainsResult(id, !removed)
      else if(element > elem) containsChild(Right, requester, id, element)
      else containsChild(Left, requester, id, element)

    case CopyTo(newRoot) =>
      if(removed && subtrees.isEmpty) context.parent ! CopyFinished
      else copyTo(newRoot)

    case CopyFinished =>

  }

  private def getChild(element: Int): ActorRef =
    if(element > elem) getSubtree(element, Right)
    else getSubtree(element, Left)

  private def getSubtree(element: Int, pos: Position): ActorRef =
    if(subtrees contains pos) subtrees(pos)
    else createSubtree(pos, element)

  private def createSubtree(pos: Position, element: Int): ActorRef = {
    subtrees = subtrees + (pos -> getDefaultNode(element))
    subtrees(pos)
  }

  private def getDefaultNode(element: Int): ActorRef = 
    context.actorOf(props(element, initiallyRemoved = true))

  private def process(requester: ActorRef, id: Int, isRemoved: Boolean): Unit = {
    removed = isRemoved
    requester ! OperationFinished(id)
  }

  private def containsChild(pos: Position, requester: ActorRef, id: Int, element: Int): Unit =
    if(subtrees.contains(pos)) subtrees(pos) ! Contains(requester, id, element)
    else requester ! ContainsResult(id, false)

  private def copyTo(newRoot: ActorRef): Unit = {
    val children = subtrees.values.toSet
    children foreach { _ ! CopyTo(newRoot) }
    
    if(!removed) newRoot ! Insert(self, self.hashCode, elem)
    else self ! OperationFinished(self.hashCode)
    
    context become copying(children, removed)
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case CopyFinished => process(expected - sender, insertConfirmed)
    
    case OperationFinished(id) => process(expected, true)
  }

  private def process(expected: Set[ActorRef], insertConfirmed: Boolean) =
    if(expected.isEmpty && insertConfirmed) restore
    else context become copying(expected, insertConfirmed)

  private def restore = {
    context.parent ! CopyFinished
    context become normal
  }


}
