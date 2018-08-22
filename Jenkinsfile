def stringsToEcho = ["a", "b", "c", "d"]

// The map we'll store the parallel steps in before executing them.
def stepsForParallel = stringsToEcho.collectEntries {
    ["echoing ${it}" : transformIntoStep(it)]
}

/*
def branches = ['3.07', '3.08.4', '3.09.3'].collectEntries {
    [$it : {
            sh "opam switch $it && eval `opam config env` && mkdir build/$it && cd build/$it && ../../configure && make && make tests"
        }]
}
*/

// Take the string and echo it.
def transformIntoStep(inputString) {
    // We need to wrap what we return in a Groovy closure, or else it's invoked
    // when this method is called, not when we pass it to parallel.
    // To do this, you need to wrap the code below in { }, and either return
    // that explicitly, or use { -> } syntax.
    return {
        node {
            echo inputString
        }
    }
}

pipeline {
    agent {
        dockerfile {
            label 'slave'
        }
    }

    stages {
        stage('Build') {
            steps {
                sh 'eval `opam config env` && autoreconf && mkdir build && cd build && ../configure && make'
            }
        }
        stage('Test') {
            steps {
                parallel stepsForParallel
            }
        }
        stage('Deploy') {
            steps {
                sh 'cd build && make dist'
                archiveArtifacts artifacts: 'build/*.tar.gz', fingerprint: true
            }
        }
    }
}
