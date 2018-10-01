/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package checkerboard;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.scene.paint.Color;
import javafx.stage.Stage;

/**
 *
 * @author wjohnke
 */
public class CheckerBoard extends Application {
    private int numRows=0;
    private int numCols=0;
    private double boardWidth=0;
    private double boardHeight=0;
    private Color lightColor;
    private Color darkColor;
    private double recWidth=0;
    private double recHeight=0;
    private GridPane gridPane;
    
    @Override
    public void start(Stage stage) throws Exception {
        FXMLLoader loader=new FXMLLoader(getClass().getResource("FXMLDocument.fxml"));
        Parent root=loader.load();
        //CheckerBoard controller=loader.getController();
        Startable controller=loader.getController();
        Scene scene = new Scene(root);
        stage.setScene(scene);
        stage.show();
        controller.start(stage);
        
    }
    public CheckerBoard(){
        
    }
    
    public CheckerBoard(int numRows, int numCols, double boardWidth, double boardHeight){
        this(numRows, numCols, boardWidth, boardHeight, Color.RED,Color.BLACK);
    }
    public CheckerBoard(int numRows, int numCols, double boardWidth, double boardHeight, Color lightColor, Color darkColor){
        this.numRows=numRows;
        this.numCols=numCols;
        this.boardHeight=boardHeight;
        this.boardWidth=boardWidth;
        this.lightColor=lightColor;
        this.darkColor=darkColor;
        recWidth=Math.ceil(boardWidth/numCols);
        recHeight=Math.ceil(boardHeight/numRows);
    }
    
    public GridPane build(){
        return gridPane;
    }
    
    public GridPane getBoard(){
        return gridPane;
    }
    
    public int getNumRows(){
        return numRows;
    }
    public int getNumCols(){
        return numCols;
    }
    public double getWidth(){
        return boardWidth;
    }
    public double getHeight(){
        return boardHeight;
    }
    public Color getLightColor(){
        return lightColor;
    }
    public Color getDarkColor(){
        return darkColor;
    }
    public double getRectangleWidth(){
        return recWidth;
    }
    public double getRectangleHeight(){
        return recHeight;
    }
    

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        launch(args);
    }
    
}
