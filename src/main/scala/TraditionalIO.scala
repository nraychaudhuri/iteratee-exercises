
import java.io._

class TraditionalIO {

  def traditionalIO(): Unit = {
    val buf = new Array[Byte](512)

    val fin = new FileInputStream("in.bin")
    fin.read(buf)
   
    val fout = new FileOutputStream("out.bin")
    fout.write(buf)
   
    fin.close()
    fout.close()
    
  }
  
}