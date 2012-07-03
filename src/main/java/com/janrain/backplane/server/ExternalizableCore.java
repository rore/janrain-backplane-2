package com.janrain.backplane.server;

import com.janrain.commons.supersimpledb.message.AbstractMessage;
import com.janrain.commons.supersimpledb.message.MessageField;
import org.apache.commons.lang.NotImplementedException;

import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * @author Tom Raney
 */
public class ExternalizableCore extends AbstractMessage implements Externalizable {

    @Override
    public void writeExternal(ObjectOutput objectOutput) throws IOException {
        HashMap<String, String> map = new HashMap<String, String>();
        Set<String> keys = this.keySet();
        Iterator it = keys.iterator();
        while (it.hasNext()) {
            String key = (String) it.next();
            map.put(key, this.get(key));
        }

        objectOutput.writeObject(map);
    }

    @Override
    public void readExternal(ObjectInput objectInput) throws IOException, ClassNotFoundException {
        this.putAll((Map<? extends String, ? extends String>) objectInput.readObject());
    }

    @Override
    public String getIdValue() {
        throw new NotImplementedException();
    }

    @Override
    public Set<? extends MessageField> getFields() {
        throw new NotImplementedException();
    }
}
