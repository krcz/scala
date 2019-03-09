import scala.annotation.tailrec

case class Zipper[A] private (node: BinTree[A], parentZipper: Option[(Zipper.Direction, Zipper[A])])

object Zipper {
  sealed trait Direction
  case object Left extends Direction
  case object Right extends Direction

  // Get a zipper focussed on the root node.
  def fromTree[A](bt: BinTree[A]): Zipper[A] = {
    Zipper(bt, None)
  }

  // Get the complete tree from a zipper.
  def toTree[A](zipper: Zipper[A]): BinTree[A] = {
    up(zipper) match {
      case Some(z) => toTree(z)
      case None => zipper.node
    }
  }

  // Get the value of the focus node.
  def value[A](zipper: Zipper[A]): A = {
    zipper.node.value
  }

  // Get the left child of the focus node, if any.
  def left[A](zipper: Zipper[A]): Option[Zipper[A]] = {
    zipper.node.left.map(leftNode => Zipper(leftNode, Some((Left, zipper))))
  }

  // Get the right child of the focus node, if any.
  def right[A](zipper: Zipper[A]): Option[Zipper[A]] = {
    zipper.node.right.map(rightNode => Zipper(rightNode, Some((Right, zipper))))
  }

  // Get the parent of the focus node, if any.
  def up[A](zipper: Zipper[A]): Option[Zipper[A]] = {
    zipper.parentZipper.map {
      case (Left, pzip) => pzip.copy(node = pzip.node.copy(left = Some(zipper.node)))
      case (Right, pzip) => pzip.copy(node = pzip.node.copy(right = Some(zipper.node)))
    }
  }

  // Set the value of the focus node.
  def setValue[A](v: A, zipper: Zipper[A]): Zipper[A] = {
    zipper.copy(node = zipper.node.copy(value = v))
  }

  // Replace a left child tree.
  def setLeft[A](l: Option[BinTree[A]], zipper: Zipper[A]): Zipper[A] = {
    zipper.copy(node = zipper.node.copy(left = l))
  }

  // Replace a right child tree.
  def setRight[A](r: Option[BinTree[A]], zipper: Zipper[A]): Zipper[A] = {
    zipper.copy(node = zipper.node.copy(right = r))
  }
}

// A binary tree.
case class BinTree[A](value: A, left: Option[BinTree[A]], right: Option[BinTree[A]])

