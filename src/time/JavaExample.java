package time;

import java.io.File;
import edu.stanford.nlp.io.IOUtils;

class JavaExample {
  public static void main(String[] args) {
    System.out.println("Hello World!");
    TreeTime model = IOUtils.readObjectFromFileNoExceptions(new File("dist/interpretModel.ser.gz"));
  }
}
