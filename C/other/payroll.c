#include <stdio.h>
#include <stdlib.h>

struct Employee {
    char*               name;
    int                 salary;
    int                 points;
    struct Employee*    supervisor;
};

/* Annual raise program. */
raise(struct Employee p[], int n) {
    printf("Raising salaries... ");
    int i;
    for (i=0; i<n; i++) {     /* Consider each employee.  */
        p->salary =     
            p->salary           /* Salary adjustment:       */
            + 100               /*   cost-of-living term,   */
            + p->points;        /*   merit term.            */
        p->points = 0;          /* Start over next year!    */
        check(p);               /* Make sure no disparities. */
        p = p + 1;              /* On to next record!       */
    }
    printf("Done.\n\n");
}

/* Make sure employee is getting less than boss: */
check(struct Employee* e) {     /* arg is pointer to record. */
    if (e == e->supervisor)     /* Ignore the president.    */
        return;
    if (e->salary <             /* Problem here?            */
        (e->supervisor)->salary)
        return;                 /* Nope, leave happy.       */
    /* When e's boss is making no more than e is,
    give boss a raise, then check that boss's
    new salary causes no additional problems: */
    (e->supervisor)->salary =
        1 + e->salary;          /* Now boss makes more.     */
    check(e->supervisor);       /* Check further.           */
}

printout(struct Employee p[], int n) {
    int i;
    for (i=0; i<n; i++) printf("%s\t\t%d\n", p[i].name, p[i].salary);
    printf("\n");
}

update(struct Employee* e, char* name, int salary, int points, struct Employee* s) {
    e->name         = name;
    e->salary       = salary;
    e->points       = points;
    e->supervisor   = s;
}

main(int argc, char** argv) {
    struct Employee payroll[5];
    update(&payroll[0], "Fring",    100, 0, &payroll[0]);
    update(&payroll[1], "JJ Hook",  100, 0, &payroll[0]);
    update(&payroll[2], "Mike",     100, 4, &payroll[1]);
    update(&payroll[3], "Walt",     50,  1, &payroll[2]);
    update(&payroll[4], "Jesse",    50,  1, &payroll[3]);

    printout(payroll, 5);

    raise(payroll, 5);

    printout(payroll, 5);

}
