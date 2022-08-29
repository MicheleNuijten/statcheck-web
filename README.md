
<p align="center">
  <img width="400px" src="https://github.com/MicheleNuijten/statcheck-web/raw/main/www/img/statcheck.png"</img>
</p>


# statcheck on the web

## What is statcheck?

statcheck is a "spellchecker" for statistics. It checks whether your p-values match their accompanying test statistic and degrees of freedom. statcheck searches for null-hypothesis significance test (NHST) in APA style (e.g., t(28) = 2.2, p < .05). It recalculates the p-value using the reported test statistic and degrees of freedom. If the reported and computed p-values don't match, statcheck will flag the result as an error.

<p align="center">
  <img max-width="600px" src="https://github.com/MicheleNuijten/statcheck-web/raw/main/www/img/faq-consistent.svg"</img>
</p>

<p align="center">
  <img max-width="600px" src="https://github.com/MicheleNuijten/statcheck-web/raw/main/www/img/faq-inconsistent.svg"</img>
</p>

<p align="center">
  <img max-width="600px" src="https://github.com/MicheleNuijten/statcheck-web/raw/main/www/img/faq-decision-inconsistent.svg"</img>
</p>

## How can I use the statcheck web app?

The statcheck web app is a simple interface that calls on the statcheck R package in the background. To use the web app, all you need is a working internet connection. You don't need R, or even know how to work with R.

Simply upload a paper in .pdf, .html, or .docx format. Statcheck will search your document for statistics and returns a table with all detected statistics and whether they are internally consistent or not. 
