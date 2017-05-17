package pageRanking;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

public class PageRank {

	private double dampingFactor;
	private Map<Integer, List<Integer>> totalGraph;
	private double initialRank;
	private Map<Integer, Double> pageRankValue;

	private Map<Integer, List<Integer>> rankMap = new HashMap<Integer, List<Integer>>();
	private List<Double> ranks = new ArrayList<Double>();

	public PageRank() {
		dampingFactor = 0.85;
		totalGraph = new HashMap<>();
		pageRankValue = new HashMap<>();

	}

	public static void main(String args[]) {
		ReadInput readInput = new ReadInput();
		PageRank pageRank = new PageRank();
		final List<String> stopListWords = readInput.loadStopList("./src/stops.txt");
		try {
			File file = new File("./src/story.txt");
			readInput.readContentFromFile(new BufferedReader(new FileReader(file)), stopListWords);

		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}

		pageRank.calculatePageRank(readInput.getForwardIndex());
		pageRank.calculatePageRank(readInput.getInvertedIndex());

		System.out.println(pageRank.totalGraph.size() + " total graph");
		// Get the first iteration page rank value.
		pageRank.initialRank = pageRank.iterateFirstLoop(pageRank.totalGraph);
		System.out.println("First iteration value " + pageRank.initialRank);
		// pageRank.calculate();
		pageRank.laterIteration(pageRank.totalGraph);
		List<Double> l = new ArrayList<>(pageRank.pageRankValue.values());
		Collections.sort(l, new Comparator<Double>() {
			@Override
			public int compare(Double c1, Double c2) {
				return Double.compare(c1, c2);
			}
		});
		l = l.subList(l.size() - 10, l.size());
		int counter = 0;

		// Top sentences
		System.out.println("Top sentences are");
		breakPointer: for (Entry<Integer, Map<Integer, String>> entry : readInput.getForwardIndex().entrySet()) {
			for (Entry<Integer, Double> en : pageRank.pageRankValue.entrySet()) {
				if (en.getKey().equals(entry.getKey()) && l.contains(en.getValue())) {
					// System.out.println("Loop");
					for (Entry<String, Integer> lineEntry : readInput.getLineNodes().entrySet()) {

						if (lineEntry.getValue().equals(en.getKey())) {
							counter++;
							System.out.println(lineEntry.getKey());
							if (counter == 10)
								break breakPointer;
						}
					}
				}
			}

		}
		// Top words
		System.out.println("Top words as they appear are :");
		counter = 0;
		BreakPointer: for (Entry<Integer, Double> en : pageRank.pageRankValue.entrySet()) {
			for (Entry<Integer, Map<Integer, String>> invertedEntry : readInput.getInvertedIndex().entrySet())
				if (en.getKey().equals(invertedEntry.getKey()) && l.contains(en.getValue())) {
					for (Entry<String, Integer> dictEntry : readInput.getDictionary().entrySet()) {
						if (dictEntry.getValue().equals(en.getKey())) {
							counter++;
							System.out.println(dictEntry.getKey());
							if (counter == 10)
								break BreakPointer;
						}
					}
				}
		}
	}

	private void calculatePageRank(Map<Integer, Map<Integer, String>> mapToIterate) {
		List<Integer> inner = new ArrayList<>();
		for (Entry<Integer, Map<Integer, String>> entry : mapToIterate.entrySet()) {
			inner = new ArrayList<>();
			for (Entry<Integer, String> eachRecord : entry.getValue().entrySet()) {
				inner.add(eachRecord.getKey());
			}
			totalGraph.put(entry.getKey(), inner);
		}

	}

	private void laterIteration(Map<Integer, List<Integer>> totalGraph) {

		for (Entry<Integer, List<Integer>> eachRecord : totalGraph.entrySet()) {

			// For each node calculate the page rank value
			// Get total outlinks for a node
			// Node is Key and outlinks are values.
			// So PR of a node is Sum of each link PR/Number of links to the
			// node.
			// PR(eachRecord.getKey()) = --> for
			// eachRecord.getValue().get(i) gives a list of outlinks
			// in the list of Links for each link identify the PR/ no. of
			// links to tht node
			if (!eachRecord.getValue().isEmpty()) {

				for (int i : eachRecord.getValue()) {
					if (!pageRankValue.containsKey(i))
						pageRankValue.put(i, ((1 - dampingFactor)
								+ (dampingFactor * (initialRank + (1 / totalGraph.get(i).size())))));
				}
			}

		}
	}

	private double iterateFirstLoop(Map<Integer, List<Integer>> totalGraph) {
		return (1.0 / totalGraph.size());

	}

}
