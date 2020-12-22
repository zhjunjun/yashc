import java.nio.charset.Charset

import io.netty.handler.codec.http.{DefaultHttpHeaders, HttpHeaders}
import io.netty.handler.codec.http.cookie.Cookie
import org.asynchttpclient.{Realm, RequestBuilder}
import org.asynchttpclient.proxy.ProxyServer
import org.asynchttpclient.request.body.generator.BodyGenerator
import org.asynchttpclient.request.body.multipart.Part
import util.IDNDomainHelpers

case class Req(run: RequestBuilder => RequestBuilder,
                props: Req.Properties = Req.Properties()) extends RequestBuilderVerbs{
  def subject = this

  // Append a transform onto the underlying AHC RequestBuilder.
  def underlying(next: RequestBuilder => RequestBuilder) = {
    Req(run.andThen(next), props)
  }

  /// Append a transform onto the underlying AHC RequestBuilder and
  /// simultaniously transform the Req.Properties.
  def underlying(nextReq: RequestBuilder => RequestBuilder,nextProps: Req.Properties => Req.Properties) = {
    Req(run andThen nextReq, nextProps(props))
  }

}

object Req {
  final case class Properties(bodyType: BodyType = NoBody, methodExplicitlySet: Boolean = false)

  trait BodyType
  final case object NoBody extends BodyType
  final case object StringBody extends BodyType
  final case object ByteArrayBody extends BodyType
  final case object EntityWriterBody extends BodyType
  final case object FileBody extends BodyType
}

trait HostVerbs {

  // Set the URL to target a specific hostname.
  def apply(host: String) = {
    val asciiSafeDomain = IDNDomainHelpers.safeConvert(host)
    Req(_.setUrl("http://%s/".format(asciiSafeDomain)))
  }

  // Set the url to target a specific hostname and port.
  def apply(host: String, port: Int) = {
    val asciiSafeDomain = IDNDomainHelpers.safeConvert(host)
    Req(_.setUrl("http://%s:%d/".format(asciiSafeDomain, port)))
  }
}

object :/ extends HostVerbs
object host extends HostVerbs

object url extends (String => Req) {

  // Set the hostname to target a specific, complete URL.
  def apply(url: String) = {
    Req(_.setUrl(RawUri(url).toString))
  }
}

trait RequestVerbs {
  def subject: Req
}

trait MethodVerbs extends RequestVerbs {
  def HEAD    = subject.setMethod("HEAD")
  def GET     = subject.setMethod("GET")
  def POST    = subject.setMethod("POST")
  def PUT     = subject.setMethod("PUT")
  def DELETE  = subject.setMethod("DELETE")
  def PATCH   = subject.setMethod("PATCH")
  def TRACE   = subject.setMethod("TRACE")
  def OPTIONS = subject.setMethod("OPTIONS")
}

trait RequestBuilderVerbs extends RequestVerbs {
  import scala.jdk.CollectionConverters._

  // Add a new body part to the request.
  def addBodyPart(part: Part) = {
    subject.underlying(_.addBodyPart(part))
  }

  // Add a new cookie to the request.
  def addCookie(cookie: Cookie) = {
    subject.underlying(_.addCookie(cookie))
  }

  // Add a new header to the request.
  def addHeader(name: String, value: String) = {
    subject.underlying(_.addHeader(name, value))
  }

  // Add a new body parameter to the request.
  def addParameter(key: String, value: String) = {
    subject.underlying(_.addFormParam(key, value))
  }

  // Add a new query parameter to the request.
  def addQueryParameter(name: String, value: String) = {
    subject.underlying(_.addQueryParam(name, value))
  }

  /// Add a new query parameter to the request without a value. This is useful
  /// for some APIs that require adding just the key to the query parameters
  def addQueryParameter(name: String) = {
    subject.underlying(_.addQueryParam(name, null))
  }


  /// Set query parameters, overwriting any pre-existing query parameters.
  /// If an empty Seq is provided for a key, we will treat that as intending
  /// for the key to be speified without _any_ values.
  def setQueryParameters(params: Map[String, Seq[String]]) = {
    subject.underlying { ahcReqBuilder =>
      ahcReqBuilder.setQueryParams(params.view.mapValues({ paramValue =>
        paramValue match {
          case Seq() =>
            // Treat this as requesting a key with no values
            Seq[String](null).toList.asJava
          case _ =>
            paramValue.toList.asJava
        }
      }).toMap.asJava)
    }
  }

  // Set the request body from a byte array.
  def setBody(data: Array[Byte]) = {
    subject.underlying(rb => rb.setBody(data), p => p.copy(bodyType = Req.ByteArrayBody))
  }

  // Set the request body using a BodyGenerator and length.
  def setBody(dataWriter: BodyGenerator, length: Long) = {
    subject.underlying(rb => rb.setBody(dataWriter), p => p.copy(bodyType = Req.EntityWriterBody))
  }

  // Set the request body using a BodyGenerator.
  def setBody(dataWriter: BodyGenerator) = {
    subject.underlying(rb => rb.setBody(dataWriter), p => p.copy(bodyType = Req.EntityWriterBody))
  }

  // Set the request body using a string.
  def setBody(data: String) = {
    subject.underlying(rb => rb.setBody(data), p => p.copy(bodyType = Req.StringBody))
  }

  // Set the request body to the contents of a File.
  def setBody(file: java.io.File) = {
    subject.underlying(rb => rb.setBody(file), p => p.copy(bodyType = Req.FileBody))
  }

  // Set the body encoding to the specified charset.
  def setBodyEncoding(charset: Charset) = {
    subject.underlying(_.setCharset(charset))
  }

  // Set the content type and charset for the request.
  def setContentType(mediaType: String, charset: Charset) = {
    subject.underlying {
      _.setHeader("Content-Type", mediaType + "; charset=" + charset).
        setCharset(charset)
    }
  }

  // Set a header
  def setHeader(name: String, value: String) = {
    subject.underlying(_.setHeader(name, value))
  }

  // Set multiple headers
  def setHeaders(headers: Map[String, Seq[String]]) = {
    subject.underlying {
      val httpHeaders: HttpHeaders = new DefaultHttpHeaders()
      headers.foreach(h => httpHeaders.add(h._1, h._2.asJava))
      _.setHeaders(httpHeaders)
    }
  }

  // Set form parameters
  def setParameters(parameters: Map[String, Seq[String]]) = {
    subject.underlying { _.setFormParams(
      parameters.view.mapValues { _.asJava: java.util.List[String] }.toMap.asJava
    ) }
  }

  // Explicitly set the method of the request.
  def setMethod(method: String) = {
    subject.underlying(_.setMethod(method), _.copy(methodExplicitlySet = true))
  }

  // Set method unless method has been explicitly set using [[setMethod]].
  def implyMethod(method: String) = {
    if (!subject.props.methodExplicitlySet) {
      subject.underlying(_.setMethod(method))
    } else {
      subject
    }
  }

  // Set the url of the request.
  def setUrl(url: String) = {
    subject.underlying(_.setUrl(url))
  }

  // Set the proxy server for the request
  def setProxyServer(proxyServer: ProxyServer) = {
    subject.underlying(_.setProxyServer(proxyServer))
  }

  // Set the virual hostname
  def setVirtualHost(virtualHost: String) = {
    subject.underlying(_.setVirtualHost(virtualHost))
  }

  // Set the follow redirects setting
  def setFollowRedirects(followRedirects: Boolean) = {
    subject.underlying(_.setFollowRedirect(followRedirects))
  }

  // Add ore replace a cookie
  def addOrReplaceCookie(cookie: Cookie) = {
    subject.underlying(_.addOrReplaceCookie(cookie))
  }

  // Set auth realm
  def setRealm(realm: Realm) = {
    subject.underlying(_.setRealm(realm))
  }
}
