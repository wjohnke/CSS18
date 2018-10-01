/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package grid;

import java.util.Random;
import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.layout.GridPane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javafx.stage.Stage;

/**
 *
 * @author dalemusser
 */
public class Grid extends Application {
    
    private final int numRows = 53;
    private final int numCols = 53;
    private final double gridWidth = 600;
    private final double gridHeight = 600;
    
    private final Color[] colors = {Color.CORAL, Color.DEEPPINK, Color.BISQUE, Color.AZURE, Color.SEAGREEN};
    
    @Override
    public void start(Stage primaryStage) {
        
        Random rn = new Random();
        
        double rectWidth = Math.ceil(gridWidth / numCols);
        double rectHeight = Math.ceil(gridHeight / numRows);

        GridPane grid = new GridPane();
        
        int numColors = colors.length;
        for (int row = 0; row < numRows; row++) {
            for (int col = 0; col < numCols; col++) {
                Color color = colors[rn.nextInt(numColors)];
                Rectangle rect = new Rectangle(rectWidth, rectHeight, color);
                
                grid.add(rect, row, col);
            }
        }
        
        Scene scene = new Scene(grid, gridWidth, gridHeight);
        
        primaryStage.setTitle("Grid");
        primaryStage.setScene(scene);
        primaryStage.show();
    }

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        launch(args);
    }
    
}
