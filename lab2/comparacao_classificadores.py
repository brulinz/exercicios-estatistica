import numpy as np
import time
import matplotlib.pyplot as plt
from sklearn.datasets import load_svmlight_file
from sklearn.metrics import accuracy_score, confusion_matrix
from sklearn.linear_model import LogisticRegression
from sklearn.neighbors import KNeighborsClassifier
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.naive_bayes import GaussianNB

# Carregar dados
X_train, y_train = load_svmlight_file("train.txt")
X_test, y_test = load_svmlight_file("test.txt")
X_train = X_train.toarray()
X_test = X_test.toarray()

classifiers = {
    'KNN': KNeighborsClassifier(n_neighbors=3, metric='euclidean'),
    'Naive Bayes': GaussianNB(),
    'LDA': LinearDiscriminantAnalysis(),
    'Logistic Regression': LogisticRegression(max_iter=1000)
}

results = {name: [] for name in classifiers}
times = {name: [] for name in classifiers}
conf_matrices = {}

# Para cada tamanho de base de treino
batch_sizes = range(1000, X_train.shape[0]+1, 1000)
for size in batch_sizes:
    Xb = X_train[:size]
    yb = y_train[:size]
    for name, clf in classifiers.items():
        # Treinamento
        clf.fit(Xb, yb)
        # Teste - mede tempo
        start = time.time()
        y_pred = clf.predict(X_test)
        end = time.time()
        acc = accuracy_score(y_test, y_pred)
        results[name].append(acc)
        times[name].append(end - start if size == X_train.shape[0] else None)  # só medir tempo final
        if size == X_train.shape[0]:
            conf_matrices[name] = confusion_matrix(y_test, y_pred)

# Plotando
plt.figure(figsize=(10,6))
for name in classifiers:
    plt.plot(batch_sizes, results[name], label=name)
plt.xlabel('Tamanho da base de treinamento')
plt.ylabel('Acurácia na base de teste')
plt.title('Desempenho dos classificadores em função do tamanho da base de treinamento')
plt.legend()
plt.grid(True)
plt.savefig('desempenho_classificadores.png')
plt.show()

# Print tempo de classificação
print("Tempo para classificar 58k exemplos (em segundos):")
for name in classifiers:
    print(f"{name}: {times[name][-1]:.2f}")

# Print melhor classificador para 1k e para 20k exemplos
print("\nDesempenho com 1k exemplos:")
for name in classifiers:
    print(f"{name}: {results[name][0]:.4f}")
print("\nDesempenho com 20k exemplos:")
for name in classifiers:
    print(f"{name}: {results[name][-1]:.4f}")

# Análise das matrizes de confusão
for name, cm in conf_matrices.items():
    print(f"\nMatriz de confusão para {name} (20k exemplos):")
    print(cm)