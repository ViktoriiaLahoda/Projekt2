**EU Economic Indicators Dashboard 2020-2024**
Interaktywna aplikacja Shiny do analizy wskaźników ekonomicznych krajów Unii Europejskiej w latach 2020-2024.

📊 O projekcie
Aplikacja analizuje związki między następującymi wskaźnikami ekonomicznymi:

Biznes - Indeks rejestracji nowych firm

R&D - Nakłady na badania i rozwój

Edu - Poziom wykształcenia populacji

Unemp - Stopa bezrobocia

Net - Dostęp do internetu

🎯 Funkcjonalności
📈 Dashboard
Interaktywny wybór kraju

Wykres trendu: porównanie wskaźników "Biznes" vs "R&D" w latach 2020-2024

Wykres bąbelkowy: porównanie średnich wskaźników między krajami

🔗 Korelacje
Macierz korelacji Pearsona między wszystkimi wskaźnikami

Interpretacja: korelacja > 0.7 = silna, < 0.3 = słaba

📉 Model Regresji
Model liniowy: Biznes ~ RD + Edu + Unemp + Net + Kraj

Analiza wpływu poszczególnych zmiennych na indeks biznesu

Pełne statystyki modelu

📋 Dane
Podgląd przetworzonych danych

Tabela interaktywna z opcjami sortowania i filtrowania

**Źródła danych:**

Wszystkie dane pochodzą z bazy Eurostat:

Business registration rates

R&D expenditure

Educational attainment

Unemployment rates

Internet access

🔧 Przetwarzanie danych
Dane są czyszczone i przygotowywane w następujących krokach:

Wczytanie: Funkcja wczytaj() pobiera dane z plików CSV, pomijając nagłówki

Czyszczenie: Funkcja czyszczenie() usuwa znaki specjalne i konwertuje na liczby

Transformacja: Konwersja z formatu szerokiego do długiego (pivot_longer)

Łączenie: Inner join łączy wszystkie zbiory danych na podstawie Kraju i Roku

Filtrowanie: Usunięcie agregatów ("European Union", "Euro area")

Standaryzacja: Skalowanie zmiennych (mean=0, sd=1) dla porównywalności

📝 Autor
Viktoriia Lahoda
Praca zaliczeniowa - Uniwersytet Warszawski
Warswa, Polska, 2025
