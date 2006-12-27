package org.jetbrains.plugins.scala.sdk;

import com.intellij.openapi.projectRoots.*;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.util.Comparing;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.ui.IdeBorderFactory;

import javax.swing.*;
import java.util.List;
import java.util.ArrayList;
import java.awt.*;

/**
 * @author ven
 */
public class ScalaSdkConfigurable implements AdditionalDataConfigurable {

  JComboBox myJavaSdkCbx;

  private Sdk myScalaSdk;

  private SdkModel.Listener myListener;
  private SdkModel mySdkModel;

  public ScalaSdkConfigurable(SdkModel sdkModel) {
    mySdkModel = sdkModel;
    myJavaSdkCbx = new JComboBox();
    reloadModel();
    myJavaSdkCbx.setRenderer(new DefaultListCellRenderer(){
      public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
        final Component listCellRendererComponent = super.getListCellRendererComponent(list, value, index, isSelected,
                                                                                       cellHasFocus);
        if (value instanceof ProjectJdk) {
          setText(((ProjectJdk)value).getName());
        }
        return listCellRendererComponent;
      }
    });

    myListener = new SdkModel.Listener() {
      public void sdkAdded(Sdk sdk) {
        reloadModel();
      }

      public void beforeSdkRemove(Sdk sdk) {
        reloadModel();
      }

      public void sdkChanged(Sdk sdk, String previousName) {
        reloadModel();
      }

      public void sdkHomeSelected(Sdk sdk, String newSdkHome) {
        reloadModel();
      }
    };
    sdkModel.addListener(myListener);
  }

  private void reloadModel() {
    DefaultComboBoxModel model = new DefaultComboBoxModel(getJavaSdks());
    myJavaSdkCbx.setModel(model);
  }

  private Sdk[] getJavaSdks() {
    Sdk[] sdks = mySdkModel.getSdks();
    List<Sdk> result = new ArrayList<Sdk>();
    for (Sdk sdk : sdks) {
      SdkType sdkType = sdk.getSdkType();
      if (Comparing.equal(sdkType, JavaSdk.getInstance()) || sdkType.getName().equals("IDEA JDK")) {
        result.add(sdk);
      }
    }

    return result.toArray(new Sdk[result.size()]);
  }

  public void setSdk(Sdk sdk) {
    myScalaSdk = sdk;
  }

  public JComponent createComponent() {
    JPanel panel = new JPanel();
    panel.add(myJavaSdkCbx);
    setupPaths();
    panel.setBorder(IdeBorderFactory.createTitledBorder("Select Java SDK"));
    return panel;
  }

  private void setupPaths() {
    final SdkModificator modificator = myScalaSdk.getSdkModificator();
    Sdk item = (Sdk) myJavaSdkCbx.getSelectedItem();
    if (item != null) {
      modificator.setSdkAdditionalData(new JavaSdkData(item.getName(), mySdkModel));
    }
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      public void run() {
        modificator.commitChanges();
      }
    });
    myScalaSdk.getSdkType().setupSdkPaths(myScalaSdk);
  }

  public boolean isModified() {
    JavaSdkData additionalData = (JavaSdkData) myScalaSdk.getSdkAdditionalData();
    return additionalData == null || !Comparing.equal(myJavaSdkCbx.getSelectedItem(), additionalData.getJavaSdkName());
  }

  public void apply() throws ConfigurationException {
    setupPaths();
  }

  public void reset() {
    SdkAdditionalData sdkAdditionalData = myScalaSdk.getSdkAdditionalData();
    if (sdkAdditionalData != null) {
      Sdk selected = JavaSdkData.findJavaSdkByName(((JavaSdkData) sdkAdditionalData).getJavaSdkName(), mySdkModel);
      myJavaSdkCbx.setSelectedItem(selected);
    }
  }

  public void disposeUIResources() {
    mySdkModel.removeListener(myListener);
  }
}