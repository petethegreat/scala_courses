import java.util.concurrent._
import scala.util.DynamicVariable

import org.scalameter._

package object scalashop extends BoxBlurKernelInterface {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def inlineBoxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {

    // TODO implement using while loops
    // start at min_x, iterate to max x

    var n_pix = 0.0
    var mean_red = 0.0
    var mean_green = 0.0
    var mean_blue = 0.0
    var mean_alpha = 0.0

    var xx = clamp(x-radius,0, src.width-1)
    while ( xx <= clamp(x + radius, 0, src.width-1)) {
      var yy = clamp(y-radius,0,src.height-1)
      while ( yy <= clamp(y + radius, 0, src.height-1)) {
        mean_red = n_pix/(n_pix +1)*mean_red + red(src(xx,yy))/(n_pix +1)
        mean_green = n_pix/(n_pix +1)*mean_green + green(src(xx,yy))/(n_pix +1)
        mean_blue = n_pix/(n_pix +1)*mean_blue + blue(src(xx,yy))/(n_pix +1)
        mean_alpha = n_pix/(n_pix +1)*mean_alpha + alpha(src(xx,yy))/(n_pix +1)
        // should we not sum alpha*red, alpha*green, etc?
        n_pix = n_pix + 1
        yy = yy + 1
       }
      xx = xx + 1
    }
    rgba(
      mean_red.toInt,
      mean_green.toInt,
      mean_blue.toInt,
      mean_alpha.toInt)
  }

  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {

    // TODO implement using while loops
    // start at min_x, iterate to max x

    var n_pix = 0
    var mean_red = 0
    var mean_green = 0
    var mean_blue = 0
    var mean_alpha = 0

    var xx = clamp(x-radius,0, src.width-1)
    while ( xx <= clamp(x + radius, 0, src.width-1)) {
      var yy = clamp(y-radius,0,src.height-1)
      while ( yy <= clamp(y + radius, 0, src.height-1)) {
        mean_red += red(src(xx,yy))
        mean_green += green(src(xx,yy))
        mean_blue += blue(src(xx,yy))
        mean_alpha += alpha(src(xx,yy))
        
        // should we not sum alpha*red, alpha*green, etc?
        n_pix +=  1
        yy += 1
       }
      xx += 1
    }
    rgba(
      mean_red/n_pix,
      mean_green/n_pix,
      mean_blue/n_pix,
      mean_alpha/n_pix)
  }
////////////////////////////////////////////////////////////////

  val forkJoinPool = new ForkJoinPool

  abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]
    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    scheduler.value.parallel(taskA, taskB)
  }

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task { taskA }
    val tb = task { taskB }
    val tc = task { taskC }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }

  // Workaround Dotty's handling of the existential type KeyValue
  implicit def keyValueCoerce[T](kv: (Key[T], T)): KeyValue = {
    kv.asInstanceOf[KeyValue]
  }
}
