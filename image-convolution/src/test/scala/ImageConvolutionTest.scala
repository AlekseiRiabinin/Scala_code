import org.scalatest.funsuite.AnyFunSuite
import ImageConvolution._


class ImageConvolutionTest extends AnyFunSuite:

  test("ArrayData get and set methods") {
    val arrayData = new ArrayData(3, 3)
    arrayData.set(1, 1, 5)
    assert(arrayData.get(1, 1) == 5)
  }

  test("bound function") {
    assert(bound(-1, 10) == 0)
    assert(bound(5, 10) == 5)
    assert(bound(10, 10) == 9)
  }

  test("convolute function with identity kernel") {
    val inputData = new ArrayData(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)
    val kernel = new ArrayData(Array(0, 0, 0, 0, 1, 0, 0, 0, 0), 3, 3)
    val outputData = convolute(inputData, kernel, 1)
    assert(outputData.dataArray.sameElements(inputData.dataArray))
  }

  test("convolute function with averaging kernel") {
    val inputData = new ArrayData(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)
    val kernel = new ArrayData(Array(1, 1, 1, 1, 1, 1, 1, 1, 1), 3, 3)
    val outputData = convolute(inputData, kernel, 9)
    val expectedOutput = Array(3, 3, 3, 4, 5, 4, 3, 3, 3)
    assert(outputData.dataArray.sameElements(expectedOutput))
  }

  test("getArrayDataFromImage and writeOutputImage functions") {
    // This test requires an actual image file to be present in the project directory.
    // For simplicity, assume the presence of "input.png" and "output.png".
    val inputFile = "input.png"
    val outputFile = "output.png"
    val dataArrays = getArrayDataFromImage(inputFile)
    writeOutputImage(outputFile, dataArrays)
    val outputDataArrays = getArrayDataFromImage(outputFile)
    assert(dataArrays.zip(outputDataArrays).forall { case (original, output) =>
      original.dataArray.sameElements(output.dataArray)
    })
  }
