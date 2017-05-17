package pageRanking;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class ReadInput {

	// Sentence containing word
	private Map<Integer, Map<Integer, String>> forwardIndex;
	// Word inside sentence
	private Map<Integer, Map<Integer, String>> invertedIndex;
	private Map<String, Integer> dictionary;
	private Map<String, Integer> lineNodes;
	private int sentenceCounter;

	public ReadInput() {
		forwardIndex = new HashMap<>();
		invertedIndex = new HashMap<>();
		dictionary = new HashMap<>();
		lineNodes = new HashMap<>();
		sentenceCounter = 0;

	}

	/**
	 * @return the forwardIndex
	 */
	public Map<Integer, Map<Integer, String>> getForwardIndex() {
		return forwardIndex;
	}

	/**
	 * @return the invertedIndex
	 */
	public Map<Integer, Map<Integer, String>> getInvertedIndex() {
		return invertedIndex;
	}

	/**
	 * @return the dictionary
	 */
	public Map<String, Integer> getDictionary() {
		return dictionary;
	}

	/**
	 * @return the lineNodes
	 */
	public Map<String, Integer> getLineNodes() {
		return lineNodes;
	}

	// Test
	public static void main(String[] args) {

		ReadInput readInput = new ReadInput();
		final List<String> stopListWords = readInput.loadStopList("./src/stops.txt");
		try {
			File file = new File("./src/story.txt");
			readInput.readContentFromFile(new BufferedReader(new FileReader(file)), stopListWords);

		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public List<String> loadStopList(String stopListPath) {

		BufferedReader reader = null;
		String line;
		List<String> stopList = new ArrayList<>();
		try {
			reader = new BufferedReader(new FileReader(stopListPath));
			while ((line = reader.readLine()) != null) {
				stopList.add(line.trim());
			}

		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return stopList;
	}

	/**
	 * 
	 * @param bufferedReader
	 * @param stopListWords
	 */
	public void readContentFromFile(BufferedReader bufferedReader, List<String> stopListWords) {

		String line;
		Map<Integer, String> tempFrwdInter;
		Map<Integer, String> tempInvrtdInter;

		try {

			// You will need to extract the set of all the words in your text
			// file,
			// after converting them to lower case and removing punctuation and
			// stop words,
			// occurring 1 per line in file

			while ((line = bufferedReader.readLine()) != null) {
				tempFrwdInter = new TreeMap<>();
				lineNodes.put(line, ++sentenceCounter);
				for (String word : line.toLowerCase().replaceAll("[\\W+_+]", " ").trim().split(" ")) {
					if (!word.isEmpty() && !stopListWords.contains(word)) {
						if (!dictionary.containsKey(word)) {
							dictionary.put(word, ++sentenceCounter);
						}
						// each sentence to each word(forward index) in your
						// dictionary

						if (!tempFrwdInter.containsKey(dictionary.get(word))) {
							tempFrwdInter.put(dictionary.get(word), word);
						}
						tempInvrtdInter = new TreeMap<>();
						// Inverted index word in sentence
						if (invertedIndex.containsKey(dictionary.get(word))) {
							tempInvrtdInter = invertedIndex.get(dictionary.get(word));
						}
						if (!tempInvrtdInter.containsKey(lineNodes.get(line))) {

							tempInvrtdInter.put(lineNodes.get(line), line);
						}
						invertedIndex.put(dictionary.get(word), tempInvrtdInter);
					}

				}
				if (!forwardIndex.containsKey(lineNodes.get(line))) {
					if (!tempFrwdInter.isEmpty()) {
						forwardIndex.put(lineNodes.get(line), tempFrwdInter);
					}
				}
			}

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

}
