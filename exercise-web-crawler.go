package main

import (
	"fmt"
	"net/http"
	"net/url"

	"golang.org/x/net/html"
)

type Page struct {
	Title string
	Links []string
}

func GetPageTitle(node *html.Node) string {
	if node.Type == html.ElementNode && node.Data == "title" {
		return node.FirstChild.Data
	}

	for c := node.FirstChild; c != nil; c = c.NextSibling {
		title := GetPageTitle(c)

		if title != "" {
			return title
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

func Crawl(url string, depth int) (Page, error) {
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

	return Page{
		Title: GetPageTitle(doc),
		Links: GetPageLinks(url, doc),
	}, nil
}

func main() {
	page1, _ := Crawl("https://golang.org/", 10)
	page2, _ := Crawl(page1.Links[3], 10)

	fmt.Println(page2)
}
