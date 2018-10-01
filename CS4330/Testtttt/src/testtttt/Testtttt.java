/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package testtttt;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import javafx.scene.input.DataFormat;

/**
 *
 * @author wjohnke
 */
public class Testtttt {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) throws MalformedURLException {
        Board b = new Board();
        Checkers c = new Checkers(8,8);
        
        b = c;
        
        
        
        java.util.ArrayList<String> names = new java.util.ArrayList<>();
names.add("Joshua"); 
java.util.ArrayList<String> people = names;
names.add("Richard"); 
people.add("Dale"); 
System.out.println(people);
        
    }
    
}
class Board{}
interface Playable{}
abstract class dasfasd{}
class Checkers extends Board implements Playable{
    Checkers(int num, int num2){
        
    }
}
