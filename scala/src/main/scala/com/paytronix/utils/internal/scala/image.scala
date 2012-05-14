//
// Copyright 2010-2012 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scala

import java.awt.{Graphics, RenderingHints, Transparency}
import java.awt.geom.AffineTransform
import java.awt.image.{AffineTransformOp, BufferedImage, ColorModel, IndexColorModel}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}
import javax.imageio.{ImageIO, ImageReader}
import javax.imageio.stream.{MemoryCacheImageInputStream, MemoryCacheImageOutputStream}
import org.slf4j.{Logger, LoggerFactory}

import com.paytronix.utils.scala.result.{Failed, Okay, Result, firstOrLast}
import com.paytronix.utils.scala.resource.withResource

/**
 * Helpers for manipulating images
 */
object image
{
    private val logger = LoggerFactory.getLogger(getClass)

    /** Rendering hints set up for the highest quality rendering */
    val highQualityHints = {
        val h = new RenderingHints(null)
        h.put(RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY)
        h.put(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY)
        h.put(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
        h.put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        h.put(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
        h
    }

    /** Run a function with the given graphics and dispose it no matter what */
    def withGraphics[A, G <: Graphics](g: G)(f: G => A): A =
        try {
            f(g)
        } finally {
            try {
                g.dispose
            } catch {
                case e => logger.warn("Failed to release Graphics:", e)
            }
        }

    /**
     * Apply a function to each ImageReader that could read the given input stream, returning the result of the first application that results in a Okay.
     * If no application results in Okay, generates (or chains) a Failed
     */
    def withImageReaders[A](inputStream: InputStream)(f: ImageReader => Result[A]): Result[A] =
        withResource(new MemoryCacheImageInputStream(inputStream)) { imageInputStream =>
            val readers = stream.stream(ImageIO.getImageReaders(imageInputStream)).force
            firstOrLast(readers) { reader =>
                reader.setInput(imageInputStream)
                try {
                    f(reader)
                } finally {
                    reader.dispose
                }
            } | ("Could not read image using any reader (tried " + readers.map(_.toString).mkString(", ") + ")")
        }

    /** Like withImageReaders, but reads from a byte array rather than any input stream */
    def withImageReaders[A](imageData: Array[Byte])(f: ImageReader => Result[A]): Result[A] =
        withResource(new ByteArrayInputStream(imageData))(bais => withImageReaders(bais)(f))

    /** Convert a ImageIO format name to the corresponding MIME type */
    val formatNameToMIMEType: PartialFunction[String, String] = {
        case "JPEG" => "image/jpeg"
        case "gif"  => "image/gif"
        case "png"  => "image/png"
    }

    /** Convert a MIME type to a file extension */
    val formatMIMETypeToFileExtension: PartialFunction[String, String] = {
        case "image/jpeg" => "jpg"
        case "image/gif" => "gif"
        case "image/png" => "png"
    }

    /** Write a BufferedImage to a byte array using the given type name */
    def imageToByteArray(source: BufferedImage, destType: String): Array[Byte] =
        withResource(new ByteArrayOutputStream()) { baos =>
            withResource(new MemoryCacheImageOutputStream(baos)) { ios =>
                ImageIO.write(source, destType, ios)
            }
            baos.toByteArray
        }


    /**
     * Resize an image of the given source type by the given ratios, properly handling GIF transparency, giving back the resized
     * image and the new image format type that should be used.
     *
     * The image type might change if the input type is an indexed color model, because it is a hard problem to choose an optimized
     * palette, and currently we don't. This function will return "png" as the new type in this case.
     *
     * If the input image is not using an indexed color model with transparency, then the target format and color model will be
     * identical to the source.
     */
    def resize(source: BufferedImage, inputFormat: String, dx: Double, dy: Double): (BufferedImage, String) = {
        var sourceColorModel = source.getColorModel
        val targetColorModel = source.getColorModel
        val standardColorModel = ColorModel.getRGBdefault

        val (targetWidth, targetHeight) = (((source.getWidth: Double) * dx).asInstanceOf[Int], ((source.getHeight: Double) * dy).asInstanceOf[Int])

        def resize(src: BufferedImage, dst: BufferedImage) {
            val g = dst.createGraphics
            try {
                g.setRenderingHints(highQualityHints)
                g.drawImage(src, new AffineTransformOp(AffineTransform.getScaleInstance(dx, dy), AffineTransformOp.TYPE_BICUBIC), 0, 0)
            } finally {
                g.dispose
            }
        }

        // GIF support in Java is very ornery. For GIFs we have to manually do the masking on input, and then just punt on outputting GIFs and instead output PNGs.
        if (sourceColorModel.isInstanceOf[IndexColorModel] &&
            sourceColorModel.hasAlpha &&
            sourceColorModel.getTransparency == Transparency.BITMASK &&
            sourceColorModel.asInstanceOf[IndexColorModel].getTransparentPixel >= 0) {

            val indexColorModel = sourceColorModel.asInstanceOf[IndexColorModel]
            val transparent = indexColorModel.getRGB(indexColorModel.getTransparentPixel)

            val masked = new BufferedImage(standardColorModel, standardColorModel.createCompatibleWritableRaster(source.getWidth, source.getHeight), standardColorModel.isAlphaPremultiplied, null)
            var w = masked.getWidth
            var h = masked.getHeight

            val buf  = new Array[Int](w)

            var y = 0
            while (y < h) {
                source.getRGB(0, y, w, 1, buf,  0, 1)

                var x = 0
                while (x < w) {
                    val c = buf(x)
                    if (c == transparent) {
                        buf(x) = 0
                    }
                    x += 1
                }

                masked.setRGB(0, y, w, 1, buf, 0, 1)
                y += 1
            }

            val resized = new BufferedImage(standardColorModel, standardColorModel.createCompatibleWritableRaster(targetWidth, targetHeight), standardColorModel.isAlphaPremultiplied, null)
            resize(masked, resized)
            (resized, "png")
        } else if (sourceColorModel.isInstanceOf[IndexColorModel]) {
            // The input color model is indexed, and we know we won't be able to generate a tolerable palette to make another indexed color model, so use sRGB and upgrade to PNG.
            val resized = new BufferedImage(standardColorModel, standardColorModel.createCompatibleWritableRaster(targetWidth, targetHeight), standardColorModel.isAlphaPremultiplied, null)
            resize(source, resized)
            (resized, "png")
        } else {
            val resized = new BufferedImage(targetColorModel, targetColorModel.createCompatibleWritableRaster(targetWidth, targetHeight), targetColorModel.isAlphaPremultiplied, null)
            resize(source, resized)
            (resized, inputFormat)
        }
    }
}
