import java.io.{BufferedWriter, File, FileWriter}
import scala.annotation.tailrec
import scala.io.Source
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory

/* MyXmlFlattener created by Hesiquio Ballines
  my implementation based on piazza discussions
  in UIC Course CS441
 */

object XMLFlattner {

  def main(args: Array[String]): Unit = {

    //Setting property so we can work with very large files
    System.setProperty("entityExpansionLimit", String.valueOf(Integer.MAX_VALUE))

    //Loading configurations
    val conf = ConfigFactory.load()
    val tags = conf.getString("tags")
    val filename = conf.getString("filename")
    val outputName = conf.getString("outputfile")

    //Creating MyXMLFlattener object
    val flattener = new MyXMLFlattener()

    //The flattener class will handle the flattening
    flattener.xmlFlatten(filename, outputName,tags)
  }



  class MyXMLFlattener{

    val logger = LoggerFactory.getLogger(classOf[MyXMLFlattener])

    def writeFile(filename: String, inputStr: String, append: Boolean): Unit =
    {
      //Creating file with filename
      //append will determine if we need to append or start a new file
      val file = new File(filename)
      val bw = new BufferedWriter(new FileWriter(file,append))
      bw.write(inputStr)
      bw.close()
      logger.info("Write Success")
    }
    def clearFile(filename: String): Unit =
    {

      //Creating file with filename/opening and clearing file with filename
      writeFile(filename,"",false)
    }

    def ignoreLine(line:String): String =
    {
      if (line.contains("<?xml") || line.contains("<dblp>") || line.contains("<!DOCTYPE") || line.contains("</dblp>"))
      {
        ""
      }
      else
      {
        line
      }
    }

    @tailrec final def checkSplit(line: String, checkList: List[String]): String = checkList match
    {
      case Nil => logger.error("NIL Reached! End tag not found!")
                  logger.error("Please check config file! Make sure all tags are present!")
                  "error"

      case e::rest => if(line.split(e).length == 0)
                      {
                        logger.info("End Tag Found!")
                        e
                      }
                      else if(line.split(e).apply(0).length < line.length)
                      {
                        logger.info("End Tag Found!")
                        e
                      }
                      else
                      {
                        checkSplit(line,rest)
                      }

    }
    def xmlFlatten(filename: String, output:String,tags:String): Unit =
    {

      logger.info("Splitting tags")
      val checkList = tags.split("\\|").toList
      writeFile(output,"",false)

      //Going line by line
      logger.info("processing...")
      for (line <- Source.fromFile(filename).getLines)
      {


        //Splitting the line into tokens
        val tokenLine = line.split(tags)

        //If the number of tokens is 0 then there was a split where the only element was the end tag
        if(tokenLine.length == 0)
        {
          //Finding out which end tag to add and adding it
          writeFile(output,checkSplit(line,checkList)  + "\n", true)
        }

          //Otherwise we want to check if the length is smaller the reason we want to check is because
          //The number of tags could still be 1, this is the caase with a lot of the /www tags
        // because they are tabbed for some reason so it is split as \t being one element

        else if(tokenLine.apply(0).length < line.length)
        {

          //Checking the different types of splits we could have
          //Also printing the proper tag based on the splits
          if(tokenLine.length == 2)
          {
            val whichSplit = checkSplit(line,checkList)
            val inputString = tokenLine.apply(0) + whichSplit + "\n" + tokenLine.apply(1)
            writeFile(output,inputString, true)
          }
          else if (tokenLine.length == 1)
          {

            writeFile(output,checkSplit(line,checkList) + "\n", true)
          }
          else if(tokenLine.length == 0)
          {
            writeFile(output,checkSplit(line,checkList)  + "\n", true)
          }

        }

        else
        {
          val ignore = ignoreLine(line)
          writeFile(output, ignore, true)
          //print(ignore)
        }
        //tokenLine.foreach(print(_))

      }
      logger.info("Done")
    }
  }

}


