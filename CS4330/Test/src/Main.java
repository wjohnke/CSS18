import java.util.*;
import java.io.*;

class Main {
    public static String LetterCapitalize(String str) {

        String [] tokens = str.split(" ");
        for(int i=0;i<tokens.length; i++){
            char [] t = tokens[i].toCharArray();
            t[0]=Character.toUpperCase(t[0]);
            tokens[i]=new String(t);
        }
        str= formString(tokens);
        return str;

    }

    public static String formString(String [] tokens){
        String formation="";
        for(String token:tokens){
            formation+=token+" ";
        }
        return formation;
    }

    public static void main (String[] args) {
        // keep this function call here
        Scanner s = new Scanner(System.in);
        System.out.print(LetterCapitalize(s.nextLine()));
    }

}
