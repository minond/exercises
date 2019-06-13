import java.util.ArrayList;
import java.util.Random;
import java.util.Scanner;

public class hangman {
  public static void main(String[] args) {
    Random rand = new Random();
    Scanner in = new Scanner(System.in);

    // Generated using https://www.randomlists.com/random-words
    String[] words = {
      "advise",
      "ruthless",
      "gorgeous",
      "glove",
      "spoon",
      "nod",
      "eatable",
      "chase",
      "point",
      "evasive",
      "trick",
      "person"
    };

    String word = words[rand.nextInt(words.length)];
    ArrayList<String> letters = new ArrayList<String>();

    for (int i = 0; i < word.length(); i++) {
      letters.add("_");
    }

    for (int i = 0; i < 50; i++) {
      System.out.printf("Guess a letter. Input your guess and press enter: ");
      String guess = in.next();

      if (guess.length() != 1) {
        System.out.println("You are only allowed to guess a single letter!");
      } else if (word.contains(guess)) {
        // Fill in all instances of `guess` that are found in `word`.
        int index = word.indexOf(guess);

        while (index >= 0) {
          letters.set(index, guess);
          index = word.indexOf(guess, index + 1);
        }
      }

      // Print word with all correct guesses filled in.
      System.out.println(String.join(" ", letters));

      // Check if we're done.
      if (!letters.contains("_")) {
        System.out.println("You win!");
        break;
      }
    }
  }
}
