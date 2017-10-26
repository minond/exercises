package main

import (
	"fmt"
	"log"
	"net/http"
	"net/url"
	"strings"

	"golang.org/x/net/html"
)

type Page struct {
	Title string
	Links []string
}

func GetPageTitle(node *html.Node) string {
	if node.Type == html.ElementNode && node.Data == "title" {
		if node.FirstChild == nil {
			return ""
		} else {
			return strings.TrimSpace(node.FirstChild.Data)
		}
	}

	for c := node.FirstChild; c != nil; c = c.NextSibling {
		title := GetPageTitle(c)

		if title != "" {
			return strings.TrimSpace(title)
		}
	}

	return ""
}

func GetPageLinks(baseUrl string, node *html.Node) []string {
	links := make([]string, 0)
	base, err := url.Parse(baseUrl)

	if err != nil {
		return links
	}

	if node.Type == html.ElementNode && node.Data == "a" {
		for _, attr := range node.Attr {
			if attr.Key == "href" {
				rel, err := url.Parse(attr.Val)

				if err != nil {
					break
				}

				links = append(links, base.ResolveReference(rel).String())
			}
		}
	}

	for c := node.FirstChild; c != nil; c = c.NextSibling {
		links = append(links, GetPageLinks(baseUrl, c)...)
	}

	return links
}

func Crawl(url string, depth int, ch chan int) (Page, error) {
	if depth == 0 {
		return Page{}, nil
	}

	res, err := http.Get(url)

	if err != nil {
		return Page{}, fmt.Errorf("Error requesting page: %v", err)
	}

	doc, err := html.Parse(res.Body)

	if err != nil {
		return Page{}, fmt.Errorf("Error creating parser: %v", err)
	}

	title := GetPageTitle(doc)
	links := GetPageLinks(url, doc)

	log.Printf(`Page "%s" (%s) with %v links at a depth of %v.`,
		title, url, len(links), depth)

	expected := len(links)
	counter := 0
	subCh := make(chan int)
	ch <- expected

	for _, link := range links {
		go Crawl(link, depth-1, subCh)
	}

	for {
		<-subCh
		counter = counter + 1

		if counter == expected {
			close(subCh)
			log.Println("Done")
		}
	}

	return Page{
		Title: title,
		Links: links,
	}, nil
}

func main() {
	ch := make(chan int)

	go Crawl("https://golang.org/", 4, ch)

	for {
		v, ok := <-ch
		log.Println(v, ok)
	}
}
