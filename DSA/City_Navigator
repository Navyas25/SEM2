#include <stdio.h>
#include <string.h>

#define MAX 10
#define INT_MAX 1000000000

int map[MAX][MAX];
char city[MAX][50];
int total;

void show() {
    printf("\nCities:\n");
    for (int i = 0; i < total; i++) {
        printf("%d. %s\n", i, city[i]);
    }
}

void add() {
    int a, b, d;

    show();

    printf("Source: ");
    scanf("%d", &a);

    printf("Destination: ");
    scanf("%d", &b);

    if (a < 0 || a >= total || b < 0 || b >= total) {
        printf("Invalid!\n");
        return;
    }

    printf("Distance (in km): ");
    scanf("%d", &d);

    map[a][b] = d;
    map[b][a] = d;

    printf("Added between %s and %s\n", city[a], city[b]);
}

void display() {
    printf("\n      ");
    for (int i = 0; i < total; i++)
        printf("%-10s", city[i]);

    printf("\n");

    for (int i = 0; i < total; i++) {
        printf("%-6s", city[i]);
        for (int j = 0; j < total; j++) {
            printf("%-10d", map[i][j]);
        }
        printf("\n");
    }
}

void path() {
    int start;

    show();
    printf("Start city: ");
    scanf("%d", &start);

    if (start < 0 || start >= total) {
        printf("Invalid!\n");
        return;
    }

    int dist[MAX], visit[MAX] = {0}, parent[MAX];

    for (int i = 0; i < total; i++) {
        dist[i] = INT_MAX;
        parent[i] = -1;
    }

    dist[start] = 0;

    for (int i = 0; i < total - 1; i++) {
        int min = INT_MAX, u;

        for (int j = 0; j < total; j++) {
            if (!visit[j] && dist[j] < min) {
                min = dist[j];
                u = j;
            }
        }

        visit[u] = 1;

        for (int v = 0; v < total; v++) {
            if (!visit[v] && map[u][v] &&
                dist[u] + map[u][v] < dist[v]) {

                dist[v] = dist[u] + map[u][v];
                parent[v] = u;
            }
        }
    }

    printf("\nFrom %s:\n", city[start]);

    for (int i = 0; i < total; i++) {
        if (i == start) continue;

        printf("To %s = %d | Path: ", city[i], dist[i]);

        int p[MAX], k = 0, t = i;

        while (t != -1) {
            p[k++] = t;
            t = parent[t];
        }

        for (int j = k - 1; j >= 0; j--) {
            printf("%s", city[p[j]]);
            if (j != 0) printf(" -> ");
        }

        printf("\n");
    }
}

int main() {
    int ch;

    printf("Number of cities: ");
    scanf("%d", &total);

    printf("Enter city names:\n");
    for (int i = 0; i < total; i++) {
        scanf("%s", city[i]);
    }

    while (1) {
        printf("\n1. Add Road\n2. Show Map\n3. Shortest Path\n4. Exit\n");
        printf("Choice: ");
        scanf("%d", &ch);

        switch (ch) {
            case 1: add(); break;
            case 2: display(); break;
            case 3: path(); break;
            case 4: return 0;
            default: printf("Wrong choice\n");
        }
    }
}
