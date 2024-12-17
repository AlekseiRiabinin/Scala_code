import java.awt.image._
import java.io.File
import javax.imageio.ImageIO


object ImageConvolution:

  class ArrayData(val dataArray: Array[Int], val width: Int, val height: Int):
    def this(width: Int, height: Int) = this(new Array[Int](width * height), width, height)
    def get(x: Int, y: Int): Int = dataArray(y * width + x)
    def set(x: Int, y: Int, value: Int): Unit = dataArray(y * width + x) = value

  def bound(value: Int, endIndex: Int): Int =
    math.max(0, math.min(value, endIndex - 1))

  def convolute(inputData: ArrayData, kernel: ArrayData, kernelDivisor: Int): ArrayData =
    val (inputWidth, inputHeight) = (inputData.width, inputData.height)
    val (kernelWidth, kernelHeight) = (kernel.width, kernel.height)
    require(kernelWidth > 0 && (kernelWidth & 1) == 1, "Kernel must have odd width")
    require(kernelHeight > 0 && (kernelHeight & 1) == 1, "Kernel must have odd height")
    val (kernelWidthRadius, kernelHeightRadius) = (kernelWidth / 2, kernelHeight / 2)

    val outputData = new ArrayData(inputWidth, inputHeight)
    for
      i <- 0 until inputWidth
      j <- 0 until inputHeight
    do
      val newValue = (for
        kw <- 0 until kernelWidth
        kh <- 0 until kernelHeight
      yield kernel.get(kw, kh) * inputData.get(
        bound(i + kw - kernelWidthRadius, inputWidth),
        bound(j + kh - kernelHeightRadius, inputHeight)
      )).sum
      outputData.set(i, j, (newValue / kernelDivisor).round.toInt)
    outputData

  def getArrayDataFromImage(filename: String): Array[ArrayData] =
    val inputImage = ImageIO.read(File(filename))
    val (width, height) = (inputImage.getWidth, inputImage.getHeight)
    val rgbData = inputImage.getRGB(0, 0, width, height, null, 0, width)
    val (reds, greens, blues) = (
      new ArrayData(width, height),
      new ArrayData(width, height),
      new ArrayData(width, height)
    )
    for
      y <- 0 until height
      x <- 0 until width
    do
      val rgbValue = rgbData(y * width + x)
      reds.set(x, y, (rgbValue >>> 16) & 0xFF)
      greens.set(x, y, (rgbValue >>> 8) & 0xFF)
      blues.set(x, y, rgbValue & 0xFF)
    Array(reds, greens, blues)

  def writeOutputImage(filename: String, redGreenBlue: Array[ArrayData]): Unit =
    val (reds, greens, blues) = (redGreenBlue(0), redGreenBlue(1), redGreenBlue(2))
    val outputImage = BufferedImage(reds.width, reds.height, BufferedImage.TYPE_INT_ARGB)
    for
      y <- 0 until reds.height
      x <- 0 until reds.width
    do
      val red = bound(reds.get(x, y), 256)
      val green = bound(greens.get(x, y), 256)
      val blue = bound(blues.get(x, y), 256)
      outputImage.setRGB(x, y, (red << 16) | (green << 8) | blue | 0xFF000000)
    ImageIO.write(outputImage, "PNG", File(filename))

  def main(args: Array[String]): Unit =
    val (kernelWidth, kernelHeight, kernelDivisor) = (args(2).toInt, args(3).toInt, args(4).toInt)
    println(s"Kernel size: $kernelWidth x $kernelHeight, divisor=$kernelDivisor")
    val kernel = new ArrayData(kernelWidth, kernelHeight)
    args.drop(5).grouped(kernelWidth).zipWithIndex.foreach { case (row, i) =>
      row.zipWithIndex.foreach { case (value, j) =>
        kernel.set(j, i, value.toInt)
      }
    }

    val dataArrays = getArrayDataFromImage(args(0)).map(convolute(_, kernel, kernelDivisor))
    writeOutputImage(args(1), dataArrays)

// sbt "run input.png output.png 3 3 9 1 1 1 1 1 1 1 1 1"
