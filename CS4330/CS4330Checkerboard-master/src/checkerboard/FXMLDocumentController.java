/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package checkerboard;

import java.net.URL;
import java.util.Random;
import java.util.ResourceBundle;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.layout.GridPane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javafx.stage.Stage;

/**
 *
 * @author wjohnke
 */
public class FXMLDocumentController implements Initializable, Startable {
    
    private Stage stage;
    private double gridWidth;
    private double gridHeight;
    public CheckerBoard current;
    @FXML private MenuItem sixteen;
    @FXML private MenuItem ten;
    @FXML private MenuItem eight;
    @FXML private MenuItem three;
    @FXML private MenuItem defaultColor;
    @FXML private MenuItem blueColor;
    
    @FXML
    private GridPane gridPane;
    
    @FXML
    private void handleResize(ActionEvent event){
        if(current!=null){
            current=new CheckerBoard(current.getNumRows(), current.getNumCols(), stage.getWidth(), stage.getHeight(), current.getLightColor(), current.getDarkColor() );
        }
    }
    @FXML
    private void handleGridChange(ActionEvent event){
        MenuItem gridChoice=(MenuItem)event.getSource();
        
        if(current!=null){
            if(gridChoice.equals(sixteen)) current=new CheckerBoard(16, 16, stage.getWidth(), stage.getHeight(), current.getLightColor(), current.getDarkColor() );
            else if(gridChoice.equals(ten)) current=new CheckerBoard(10, 10, stage.getWidth(), stage.getHeight(), current.getLightColor(), current.getDarkColor() );
            else if(gridChoice.equals(three)) current=new CheckerBoard(3, 3, stage.getWidth(), stage.getHeight(), current.getLightColor(), current.getDarkColor() );
            else current=new CheckerBoard(8, 8, stage.getWidth(), stage.getHeight(), current.getLightColor(), current.getDarkColor() );
            
            refresh(current);
        }
    }
    @FXML
    private void handleColorChange(ActionEvent event){
        if(current!=null){
            if( ((MenuItem)event.getSource()).equals(blueColor) ) current=new CheckerBoard(current.getNumRows(), current.getNumCols(), stage.getWidth(), stage.getHeight(), Color.SKYBLUE, Color.DARKBLUE );
            else current=new CheckerBoard(current.getNumRows(), current.getNumCols(), stage.getWidth(), stage.getHeight());
            
            refresh(current);
        }
    }    

    @Override
    public void start(Stage stage) {
        this.stage=stage;
        current=new CheckerBoard(8, 8, stage.getWidth(), stage.getHeight());
        ChangeListener<Number> changeListener = (ObservableValue<? extends Number> observable, Number oldValue, final Number newValue) -> {
            refresh(current);
        };
        
        this.stage.widthProperty().addListener(changeListener);
        this.stage.heightProperty().addListener(changeListener);
        
        refresh(current);
    }
    private void refresh(CheckerBoard current){
        clearGridPane();
        
        current.build();
        
        gridWidth=gridPane.getWidth();
        gridHeight=gridPane.getHeight();
        int numRows=current.getNumRows();
        int numCols=current.getNumCols();
        double rectWidth = Math.ceil(gridWidth / numCols);
        double rectHeight = Math.ceil(gridHeight / numRows);
        
        for (int row = 0; row < numRows; row++) {
            for (int col = 0; col < numCols; col++) {
                Color color = ((row+col)%2)==0? current.getLightColor() : current.getDarkColor();
                Rectangle rect = new Rectangle(rectWidth, rectHeight,color);
                gridPane.add(rect, row, col);
            }
        }
    }
    private void clearGridPane(){
        gridPane.getChildren().clear();
    }
    
    
    @Override
    public void initialize(URL url, ResourceBundle rb) {
        // TODO
    }
}
