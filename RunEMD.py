from EvolutionaryModelDiscovery import *
modelPath = "SimpleSchellingTwoSubgroups_HatnaAdaption.nlogo"
setup = ['setup']
measurements = ["c-index", "ticks"]
ticks = 10
emd = EvolutionaryModelDiscovery(modelPath,setup, measurements, ticks)
emd.setMutationRate(0.1)
emd.setCrossoverRate(0.8)
emd.setGenerations(10)
def cindexObjective(results):
    #print(results.iloc[-1][0])
    return results.iloc[-1][0]
emd.setObjectiveFunction(cindexObjective)
if __name__ == '__main__':
    emd.evolve()