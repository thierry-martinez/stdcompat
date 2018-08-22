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
            parallel {
                stage('3.07') {
                    steps {
                        sh 'opam switch 3.07 && eval `opam config env` && mkdir build/3.07 && cd build/3.07 && ../../configure && make && make tests'
                    }
                }
                stage('3.08.4') {
                    steps {
                        sh 'opam switch 3.08.4 && eval `opam config env` && mkdir build/3.08.4 && cd build/3.08.4 && ../../configure && make && make tests'
                    }
                }
            }
        }
        stage('Deploy') {
            steps {
                sh 'make dist'
                archiveArtifacts artifacts: '*.tar.gz', fingerprint: true
            }
        }
    }
}
