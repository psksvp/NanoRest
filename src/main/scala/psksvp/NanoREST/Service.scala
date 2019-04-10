/*
 *  The BSD 3-Clause License
 *  Copyright (c) 2018. by Pongsak Suvanpong (psksvp@gmail.com)
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without modification,
 *  are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *  this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright notice,
 *  this list of conditions and the following disclaimer in the documentation
 *  and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the copyright holder nor the names of its contributors may
 *  be used to endorse or promote products derived from this software without
 *  specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 *  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 *  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 *  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 *  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 *  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * This information is provided for personal educational purposes only.
 *
 * The author does not guarantee the accuracy of this information.
 *
 * By using the provided information, libraries or software, you solely take the risks of damaging your hardwares.
 */

package psksvp.NanoREST

import org.nanohttpd.protocols.http.IHTTPSession
import org.nanohttpd.protocols.http.NanoHTTPD
import org.nanohttpd.protocols.http.response.{Response, Status}
import org.nanohttpd.protocols.http.request.Method


abstract class Service(val name:String, val port:Int = 8080) extends NanoHTTPD(port)
{
  private val routes = scala.collection.mutable.ListBuffer[Route]()
  private val parameters = scala.collection.mutable.Map[String, Seq[String]]()

  def run():Unit =
  {
    start(NanoHTTPD.SOCKET_READ_TIMEOUT, false)
    println(s"Start $name on port $port")
  }

  case class Path(path:String)
  {
    lazy val components:Array[String] = path.split("/").filter(_.nonEmpty)
  }

  case class Route(path:Path, method:Option[Method])(generator: => Response)
  {
    lazy val components:Array[String] = path.components
    def response:Response = generator
  }

  /////////////////////////////////////////////////////////////////////////
  /// method handler DSL
  /////////////////////////////////////////////////////////////////////////
  def route(path:String, method:Option[Method])(block:  => Response):Unit =
  {
    routes.append(Route(Path(path), method)(block))
  }

  def get(path:String)(block:  => Response):Unit = route(path, Some(Method.GET))(block)
  def post(path:String)(block: => Response):Unit = route(path, Some(Method.POST))(block)
  def put(path:String)(block: => Response):Unit = route(path, Some(Method.PUT))(block)


  /////////////////////////////////////////////////////////////////////////
  /// response DSL
  /////////////////////////////////////////////////////////////////////////
  def response(text:String):Response =
  {
    Response.newFixedLengthResponse(Status.OK, "text/plain", text)
  }

  def response(mine:String, content:String):Response =
  {
    Response.newFixedLengthResponse(Status.OK, mine, content)
  }

  def response(mine:String, content:Array[Byte]):Response =
  {
    Response.newFixedLengthResponse(Status.OK, mine, content)
  }

  lazy val noneResponse : Response =
  {
    Response.newFixedLengthResponse(Status.NO_CONTENT, "text/plain", "NONE")
  }

  lazy val okResponse : Response = response("OK")

  override def toString:String =
  {
    routes.toString
  }

  /////////////////////////////////////////////////////////////////////////
  def colon(key:String):Option[Seq[String]] = parameters.get(s":$key")
  def data(key:String):Option[Seq[String]] = parameters.get(key)
  def wildcard:Option[Seq[String]] = parameters.get("*")

  /////////////////////////////////////////////////////////////////////////
  private def matchRoute(route:Route, path:Path):Boolean =
  {
    //println(s"$self matchWith $otherPath")
    if (route.components.length != path.components.length)
      false
    else
    {
      val localParams = scala.collection.mutable.Map[String, Seq[String]]()
      localParams("*") = Seq[String]()

      val compares = for ((a, b) <- route.components zip path.components) yield
      {
        a match
        {
          case "*"              => localParams("*") = localParams("*") :+ b
                                   true
          case _ if ':' == a(0) => if(localParams.contains(a)) localParams(a) = localParams(a) :+ b
                                   else localParams(a) = Seq(b)
                                   true
          case _                => a == b
        }

      }
      val r = compares.reduceLeft(_ & _)
      if(r)
        parameters ++= localParams
      r
    }
  }

  override def serve(session: IHTTPSession): Response =
  {
    import scala.collection.JavaConverters._
    val method = session.getMethod
    val path = Path(session.getUri)
    parameters.clear()

    //println(s"--> ${session.getParameters.size()}")
    for((k, v) <- session.getParameters.asScala)
    {
      parameters(k) = v.asScala
    }
    //println(s"params: $parameters")

    val responses = (for(r <- routes) yield
                    {
                      (matchRoute(r, path), r.method) match
                      {
                        case (true, Some(m)) if m == method => Some(r.response)
                        case (true, None)                   => Some(r.response)
                        case _                              => None
                      }
                    }).flatten

    //val responses = for(r <- routes if matchRoute(r, path) && r.method == method) yield r.response
    if(responses.nonEmpty)
      responses.head
    else
      noneResponse
  }
}
