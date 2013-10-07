//
// Copyright 2012-2013 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scm.utility

import org.slf4j.LoggerFactory

import com.paytronix.utils.internal.scm.common.context.Segment.stringOps
import com.paytronix.utils.internal.scm.common.context.{QualifiedSegment, Path, root}
import com.paytronix.utils.scala.result.{Result, optionOps, tryCatch}

object addresses {
    private implicit val logger = LoggerFactory.getLogger(getClass)

    def setSiteInfo(siteName: String, serverName: String, instanceName: String): Unit = {
        _siteName = Some(siteName)
        _serverName = Some(serverName)
        _instanceName = Some(instanceName)
    }

    private var _siteName: Option[String] = None
    private var _serverName: Option[String] = None
    private var _instanceName: Option[String] = None

    def sitePath:     Result[Path] = (_siteName.toResult | "no site information set") map { site => root / ("site" ~ site) }
    def serverPath:   Result[Path] = for { server   <- _serverName.toResult   | "no site information set"; site   <- sitePath   } yield site / ("server" ~ server)
    def instancePath: Result[Path] = for { instance <- _instanceName.toResult | "no site information set"; server <- serverPath } yield server / ("instance" ~ instance)

    // Temporary solution for Issue26191: Mega FIXME
    private def parentMerchant(merchantId: Int): Int = merchantId match {
        case wellsfargo if (wellsfargo > 10000) && (wellsfargo < 20000) => 10000
        case positouch  if (positouch  > 20000) && (positouch  < 30000) => 20000
        case _                                                          =>     2
    }

    private def parentPathFragment(parentMerchantId: Int): Path = ("parent" ~ parentMerchantId.toString)
    private def merchantPathFragment(merchantId: Int): Path     = ("merchant" ~ merchantId.toString)

    def siteMerchantPath(merchantId: Int): Result[Path] =
        sitePath map { _ / parentPathFragment(parentMerchant(merchantId)) / merchantPathFragment(merchantId) }

    def merchantPath(merchantId: Int): Path =
        root / parentPathFragment(parentMerchant(merchantId)) / merchantPathFragment(merchantId)

    def pathForStore(merchantId: Int, storeCode: String): Path =
        merchantPath(merchantId) / ("store" ~ storeCode) // FIXME store group?

    def cardTemplatePath(merchantId: Int, cardTemplateCode: Int): Path =
        merchantPath(merchantId) / ("cardTemplate" ~ cardTemplateCode.toString)

    def cardTemplateTierPath(merchantId: Int, cardTemplateCode: Int, tierCode: Int): Path =
        cardTemplatePath(merchantId, cardTemplateCode) / ("tier" ~ tierCode.toString)

    def merchantFromPath(path: Path): Option[Int] =
        path.segments.length match {
            case l if l > 0 => path.segments.last match {
                case QualifiedSegment("merchant", mid) =>
                    tryCatch.value(mid.toInt).toOption
                case _ =>
                    None
            }
            case _ =>
                None
        }

    def merchantCardTemplateFromPath(path: Path): List[Option[Int]] =
        path.segments.length match {
            case l if l > 1 => path.segments.last match {
                case QualifiedSegment("cardTemplate", ctc) =>
                    List(merchantFromPath(Path(path.segments.dropRight(1)))) ++ List(tryCatch.value(ctc.toInt).toOption)
                case _ =>
                    List(merchantFromPath(path)) ++ List(None)
            }
            case _ =>
                List(merchantFromPath(path)) ++ List(None)
        }

    def merchantCardTemplateTierFromPath(path: Path): List[Option[Int]] =
        path.segments.length match {
            case l if l > 2 => path.segments.last match {
                case QualifiedSegment("tier", tc) =>
                    merchantCardTemplateFromPath(Path(path.segments.dropRight(1))) ++ List(tryCatch.value(tc.toInt).toOption)
                case _ =>
                    merchantCardTemplateFromPath(path) ++ List(None)
            }
            case _ =>
                merchantCardTemplateFromPath(path) ++ List(None)
        }

}
