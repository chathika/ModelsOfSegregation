from EvolutionaryModelDiscovery import EvolutionaryModelDiscovery
import numpy as np
import random
modelPath = "SimpleSchellingTwoSubgroups_Hatna.nlogo"
setup = [
    'set neighborhood-distance 1',
    'set prob-of-relocation-attempt-by-happy 0.01',
    'set fraction-of-blue 0.5',
    'set density 0.95',
    'set threshold-group-A 0.4',
    'set history-length 10',
    'setup']

all_setup_commands = []
for j in range(5):
    i = random.uniform(0.3,0.8)
    s = setup.copy()
    s[4] = f'set threshold-group-A {i}'
    all_setup_commands.append(s)



measurements = ["c-index"]
ticks = 500
emd = EvolutionaryModelDiscovery("/root/EMD/NetLogo-6.2.2-64/NetLogo 6.2.2", modelPath,all_setup_commands, measurements, ticks, agg_func = np.mean)
emd.set_mutation_rate(0.2)
emd.set_crossover_rate(0.8)
emd.set_generations(20)
emd.set_replications(1)
emd.set_depth(4,20)
emd.set_population_size(20)
emd.set_is_minimize(False)

def c_index_objective(results):
    return np.mean(results.iloc[-100:,0])

emd.set_objective_function(c_index_objective)

if __name__ == '__main__':
    emd.evolve()
    