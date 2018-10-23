package main

import (
	"fmt"
	"log"
	"net/http"
	"strings"
)

func main() {
	logs := strings.Repeat("Those are some logs\n", 1000)
	logs += strings.Repeat("Another bunch\n", 500)
	logs += strings.Repeat("This is an error\n", 500)
	http.HandleFunc("/logs", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		fmt.Fprint(w, logs)
	})
	log.Fatal(http.ListenAndServe(":8080", nil))
}
