package time;

import java.io.File;
import java.io.FileInputStream;
import java.io.BufferedInputStream;
import java.io.ObjectInputStream;

public class JITime {
	
	private CKYParser impl;

	public JITime(CKYParser parser){
		this.impl = parser;
	}

	public JITime(File serializedParser){
		try{
			this.impl = (CKYParser) new ObjectInputStream(new BufferedInputStream(
				new FileInputStream(serializedParser))).readObject();
		} catch (Exception e){
			throw new RuntimeException(e);
		}
	}


}
