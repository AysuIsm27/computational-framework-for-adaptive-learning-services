## Adaptive Learning Services

**Chen et al.** present a sequencing service grounded in a modified Item Response Theory (IRT)
model. The central objective is to recommend the *optimal* next learning unit, meaning one that
balances difficulty and learnability rather than being too easy or overwhelmingly hard.

- **Input:** Student's current ability estimate and descriptive metadata per unit (e.g. difficulty level).
- **Model:** Modified IRT using ability, difficulty, and discrimination values to estimate the challenge level of each unit.
- **Candidates:** Units not yet completed by the student and for which all prerequisite conditions are satisfied.
- **Best:** The unit whose predicted success likelihood is closest to a target threshold defined within the modified IRT model.

---

**Jagan et al.** propose a learner model for personalised e-content generation, applying Latent
Dirichlet Allocation (LDA) to interaction data to identify latent behavioural patterns, which are
then integrated into a detailed learner profile for subsequent personalisation.

- **Input:** Interaction records and activity data capturing initial characteristics and behavioural evidence.
- **Model:** A Learner Behaviour (LB) model based on LDA that uncovers latent patterns; behavioural topics are combined with a domain ontology to produce processed learner profiles.
- **Candidates:** Content concepts defined in the domain ontology, each evaluated against the learner's behavioural profile.
- **Best:** The concept with the highest suitability score relative to the learner's inferred behavioural traits; its associated e-content is returned as output.

---

**Rodríguez-Martínez et al.** present a personalised homework system to improve fifth-grade
students' understanding of fractions, integrating formative assessment with learning analytics
to adapt practice problems to each student's current needs.

- **Input:** Results from class-based formative assessments via an Audience Response System (ARS), recording responses to fraction tasks and identifying misconceptions or unmastered constructs.
- **Model:** A learning analytics model tracking performance across five fraction constructs (part-whole, ratio, operator, quotient, and measure), representing mastery, non-mastery, and errors.
- **Candidates:** A repository of fraction problems, each tagged to one or more constructs, filtered to those relevant to the student's weak areas after each session.
- **Best:** Problems most closely matching the student's current areas of non-mastery, balancing relevance (targeting weak constructs) and likely effectiveness.
