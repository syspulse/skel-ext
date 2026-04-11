package io.syspulse.ext.sentinel.kuba.api

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import com.typesafe.scalalogging.Logger
import spray.json._
import java.io.{File, FileWriter, BufferedWriter}

object DuneApi {
  private val log = Logger(getClass.getName)

  private val BASE_URL = "https://api.dune.com/api/v1"
  private val PAGE_SIZE = 1000

  case class QueryMetadata(totalRows: Int)

  // Get query results metadata to determine total rows
  def getQueryMetadata(queryId: String, apiKey: String): Try[QueryMetadata] = Try {
    val url = s"${BASE_URL}/query/${queryId}/results"

    log.info(s"Fetching query metadata for queryId=${queryId}")

    val response = requests.get(
      url,
      headers = Map(
        "X-Dune-API-Key" -> apiKey
      ),
      params = Map(
        "limit" -> "1",
        "offset" -> "0"
      ),
      readTimeout = 60000,
      connectTimeout = 30000
    )

    if (response.statusCode == 200) {
      val json = response.text().parseJson.asJsObject

      // Extract total row count from metadata
      val totalRows = json.fields.get("result") match {
        case Some(resultObj: JsObject) =>
          resultObj.fields.get("metadata") match {
            case Some(metadata: JsObject) =>
              metadata.fields.get("total_row_count") match {
                case Some(JsNumber(count)) => count.toInt
                case _ =>
                  log.warn(s"total_row_count not found in metadata, trying result_set_bytes")
                  // Fallback: estimate from result_set_bytes or use a large number
                  metadata.fields.get("result_set_bytes") match {
                    case Some(JsNumber(bytes)) =>
                      // Estimate: assume average 100 bytes per row
                      (bytes.toInt / 100).max(1000)
                    case _ =>
                      log.warn("Could not determine total rows, will paginate until empty")
                      Int.MaxValue
                  }
              }
            case _ => throw new Exception(s"metadata not found in response: ${response.text()}")
          }
        case _ => throw new Exception(s"result not found in response: ${response.text()}")
      }

      log.info(s"Query ${queryId} has ${totalRows} total rows")
      QueryMetadata(totalRows)
    } else {
      throw new Exception(s"Dune API error (${response.statusCode}): ${response.text()}")
    }
  }

  // Fetch a single page of results in CSV format
  def fetchResultsPageCsv(queryId: String, apiKey: String, offset: Int, limit: Int = PAGE_SIZE): Try[String] = Try {
    val url = s"${BASE_URL}/query/${queryId}/results/csv"

    log.info(s"Fetching results for queryId=${queryId}, offset=${offset}, limit=${limit}")

    val response = requests.get(
      url,
      headers = Map(
        "X-Dune-API-Key" -> apiKey
      ),
      params = Map(
        "limit" -> limit.toString,
        "offset" -> offset.toString
      ),
      readTimeout = 120000,
      connectTimeout = 30000
    )

    if (response.statusCode == 200) {
      response.text()
    } else {
      throw new Exception(s"Dune API error (${response.statusCode}): ${response.text()}")
    }
  }

  // Fetch all results and write to file in streaming fashion
  def fetchAllResults(queryId: String, apiKey: String, outputFile: String): Try[Int] = {
    log.info(s"Starting to fetch all results for queryId=${queryId} to file=${outputFile}")

    // First, get metadata to determine how many pages we need
    getQueryMetadata(queryId, apiKey) match {
      case Success(metadata) =>
        val totalRows = metadata.totalRows
        val totalPages = if (totalRows == Int.MaxValue) {
          -1 // Unknown, will paginate until empty
        } else {
          (totalRows + PAGE_SIZE - 1) / PAGE_SIZE
        }

        log.info(s"Total rows: ${totalRows}, estimated pages: ${if (totalPages == -1) "unknown" else totalPages}")

        // Open file for writing (append mode)
        val file = new File(outputFile)
        val fileExists = file.exists()
        val writer = new BufferedWriter(new FileWriter(file, true)) // append mode

        try {
          var offset = 0
          var linesWritten = 0
          var hasMoreData = true
          var isFirstPage = !fileExists // Write header only if file doesn't exist

          while (hasMoreData) {
            fetchResultsPageCsv(queryId, apiKey, offset, PAGE_SIZE) match {
              case Success(csvData) =>
                if (csvData.trim.isEmpty) {
                  log.info(s"No more data at offset ${offset}, stopping")
                  hasMoreData = false
                } else {
                  val lines = csvData.split("\n")

                  // Skip header line for subsequent pages
                  val dataLines = if (isFirstPage) {
                    lines
                  } else {
                    if (lines.length > 1) lines.drop(1) else Array.empty[String]
                  }

                  dataLines.foreach { line =>
                    if (!line.trim.isBlank) {
                      writer.write(line)
                      writer.newLine()
                      linesWritten += 1
                    }
                  }
                  writer.flush()

                  log.info(s"Wrote ${dataLines.length} lines (total: ${linesWritten} lines)")

                  // Check if we got less than PAGE_SIZE rows (last page)
                  if (lines.length - 1 < PAGE_SIZE) {
                    log.info(s"Received ${lines.length - 1} rows (less than page size), this was the last page")
                    hasMoreData = false
                  }

                  offset += PAGE_SIZE
                  isFirstPage = false
                }

              case Failure(e) =>
                writer.close()
                return Failure(e)
            }
          }

          writer.close()
          log.info(s"Successfully wrote ${linesWritten} lines to ${outputFile}")
          Success(linesWritten)

        } catch {
          case e: Exception =>
            writer.close()
            Failure(e)
        }

      case Failure(e) =>
        log.error(s"Failed to get query metadata: ${e.getMessage}")
        Failure(e)
    }
  }
}
