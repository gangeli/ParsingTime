package time;

import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.util.*;
import java.util.*;

class CoreMaps {
  public static CoreLabel merge(CoreLabel base, CoreLabel toBeMerged){
    //(variables)
    CoreLabel rtn = new CoreLabel(base.size());
    //(copy base)
    for(Class key : base.keySet()){
      rtn.set(key,base.get(key));
    }
    //(merge)
    for(Class key : toBeMerged.keySet()){
      rtn.set(key,toBeMerged.get(key));
    }
    //(return)
    return rtn;
  }

  public static CoreMap merge(CoreMap base, CoreMap toBeMerged){
    //(variables)
    CoreMap rtn = new ArrayCoreMap(base.size());
    //(copy base)
    for(Class key : base.keySet()){
      rtn.set(key, base.get(key));
    }
    //(merge)
    for(Class key : toBeMerged.keySet()){
      rtn.set(key,toBeMerged.get(key));
    }
    //(return)
    return rtn;
  }
}
