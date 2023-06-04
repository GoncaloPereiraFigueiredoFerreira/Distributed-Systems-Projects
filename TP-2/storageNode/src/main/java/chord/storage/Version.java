package chord.storage;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class Version {
    private final List<Dependencie> dependencies;
    private final String value;

    public Version(List<Dependencie> dependencies, String value) {
        this.dependencies = dependencies;
        this.value = value;
    }

    public List<Dependencie> getDependencies() {
        return dependencies;
    }

    public String getValue() {
        return value;
    }

    public String toString(){
        StringBuilder depString = new StringBuilder();
        if(dependencies.isEmpty())
            depString.append("empty");
        else {
            for (int i=0;i<dependencies.size();i++){
                depString.append(dependencies.get(i).toString());
                if(i!=dependencies.size()-1)
                    depString.append("|");
            }
        }

        return depString +"|"+ value;
    }

    public static Version fromStrings(String[] inputs) {
        List<String> partsList = Arrays.stream(inputs).toList();
        List<Dependencie> dependencieList = new ArrayList<>();

        if(partsList.size()>2) {
            for (int i = 0; i < partsList.size() - 1; i += 2) {
                String key = partsList.get(i);
                int version = Integer.parseInt(partsList.get(i + 1));
                dependencieList.add(new Dependencie(key, version));
            }
        }
        String value = partsList.get(partsList.size() - 1);

        return new Version(dependencieList, value);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Version version = (Version) o;
        return Objects.equals(dependencies, version.dependencies) && Objects.equals(value, version.value);
    }
}
